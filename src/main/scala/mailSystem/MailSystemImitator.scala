package mailSystem

import akka.actor._
import com.typesafe.config.ConfigFactory
import mailSystem.dao.MyGlobalConfig
import mailSystem.entity.{Item, Mail, PersonalMail, Player, SystemMail}
import mailSystem.service.{ItemService, MailService, PlayerService}
import mailSystem.utils.{JedisHelper, MapBean, MapBeanUtils, MyUtils}

import java.lang.Thread.sleep
import java.sql.{SQLException, SQLIntegrityConstraintViolationException}
import java.time.LocalDateTime
import scala.collection.convert.ImplicitConversions.`map AsScala`
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random

object MailSystemImitator {

  case class RequestGetClientActor(playerId: Long)

  case class ReceiveClientActor(client: ActorRef)

  case class RequestAddPlayer(playerId: Long, client: ActorRef)

  case class RequestDelPlayer(playerId: Long)

  case class RequestSendMail(sender: Long, receiver: Long, mail: PersonalMail)

  case class SendSystemMail(mail: SystemMail)

  case class RequestLoadMails(playerId: Long)

  case class RequestReadMail(playerId: Long, mailId: Long)

  case class RequestCollectAttachment(playerId: Long, mailId: Long)

  case class RequestDelMail(playerId: Long, mailId: Long)

  case class ReceiveReport(msg: String)

  case class ReceiveMails(mails: List[Mail])

  case class ReceiveObtainItems(items: Map[Item, Int])

  case class ReceiveCollectSuccess(mailId: Long)

  case class ReceiveDeleteSuccess(mailId: Long)

  case class Log(content: String)

  def playerId2Key(playerId: Long): String = s"player:$playerId"
  def systemMailId2Key(systemMailId: Long): String = s"system_mail:$systemMailId"
  def distributionKeyForAttachmentCollect(playerId: Long, mailId: Long): String = s"distribution:attachment:collect:$playerId:$mailId"
  def distributionKeyForLoadSystemMail: String = "distribution:load:system:mail"
  def distributionKeyForStatusChange: String = "distribution:status:change"
  def keyForSystemMail = "system_mail"
  def keyForMailsRead = "mails_read"
  def keyForMailsCollect = "mails_collect"

  def key2PlayerId(key: String): Option[Long] = {
    key match {
      case key if key.startsWith("player:") => Some(key.split(":")(1).toLong)
      case _ => None
    }
  }

  var serverActor: ActorRef = _
  val clients: mutable.Map[Long, ActorRef] = mutable.Map[Long, ActorRef]()

  private def myLog(content: String): Unit = {
    content match {
      case msg if msg.startsWith("process") => println(s"\u001B[32m $msg \u001B[0m")
      case _ => println(s"\u001B[31m $content \u001B[0m")
    }
  }


  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.parseString( """
        |akka {
        |  actor {
        |    allow-java-serialization = on
        |    serialize-messages = on
        |    provider = "akka.remote.RemoteActorRefProvider"
        |  }
        |  remote {
        |    artery {
        |      canonical.hostname = "127.0.0.1"
        |      canonical.port = 2552
        |    }
        |  }
        |}
        |""".stripMargin)
    val system = ActorSystem("MyMailSystem",config)
    serverActor = system.actorOf(Props(new ServerActor(clients)), "serverActor")
    myLog("新增服务端" + serverActor.path)

    def randomPlayerId = MailSystemHelper.randomPlayerId

    def randomMailIdForConcretePlayer(playerId: Long) = {
      val mails = MailService.mails(playerId)
      MyUtils.getRandom(mails).getMailId
    }

    val fixedPlayer = PlayerService.getPlayer(randomPlayerId)

    val fixedMails = MailService.mails(fixedPlayer.getPlayerId)

    def randomMailIdForFixedPlayerId = randomMailIdForConcretePlayer(fixedPlayer.getPlayerId)

    val fixedMailId = randomMailIdForFixedPlayerId

    def randomPlayerIdOnLine = clients.keySet.toSeq(Random.nextInt(clients.size))

    def randomClientActor = clients(randomPlayerIdOnLine)

    def addRandomClient(): Unit = addConcreteClient(randomPlayerId)

    def addConcreteClient(playerId: Long): Unit = {
      val clientActor = system.actorOf(Props(new ClientActor(playerId, serverActor, mutable.Map[Long, SystemMail](), mutable.Map[Long, PersonalMail](), mutable.Map[Long, PersonalMail]())), s"clientActor-$playerId")
      myLog("新增客户端" + clientActor.path)
      clientActor ! RequestAddPlayer(playerId, clientActor)
    }

    def delRandomClient(): Unit = {
      val playerId = randomPlayerIdOnLine
      delConcreteClient(playerId)
    }

    def delConcreteClient(playerId: Long): Unit = {
      clients(playerId) ! RequestDelPlayer(playerId)
    }

    val scheduler = system.scheduler

    def syncRead2DBRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => syncMailsRead())

    def clientConnectRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => addRandomClient())

    def readMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
      val playerId = randomPlayerIdOnLine
      val mailId = randomMailIdForConcretePlayer(playerId)
      val clientActor = clients(playerId)
      clientActor ! RequestReadMail(playerId, mailId)
    })

    def collectAttachmentRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
      val playerId = randomPlayerIdOnLine
      val mailId = randomMailIdForConcretePlayer(playerId)
      val clientActor = clients(playerId)
      clientActor ! RequestCollectAttachment(playerId, mailId)
    })

    def sendMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
      val sender = randomPlayerIdOnLine
      val receiver =  randomPlayerIdOnLine
      val clientActor = clients(sender)
      clientActor ! RequestSendMail(sender , receiver, new PersonalMail(sender, receiver, "title", "content", "{}", "{}"))
    })

    def sendFixedMailRegularly(rate: FiniteDuration, senderId: Long, receiverId: Long) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
      val clientActor = clients(senderId)
      clientActor ! RequestSendMail(senderId, receiverId, new PersonalMail(senderId, receiverId, "title", "content", "{}", "{}"))
    })

    def sendSystemMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
      serverActor ! SendSystemMail(new SystemMail("title", "content"))
    })

    def delMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
      val playerId = randomPlayerIdOnLine
      val mailId = randomMailIdForConcretePlayer(playerId)
      val clientActor = clients(playerId)
      clientActor ! RequestDelMail(playerId, mailId)
    })

//    for (_ <- 1 to 1) addRandomClient()
//    sleep(1000)
//    val schedule = sendMailRegularly


//    val schedule = readMailRegularly(2.seconds)
//    val schedule2 = syncRead2DBRegularly(10.seconds)
//    val schedule = sendMailRegularly(10.seconds)
    addConcreteClient(532125159977385984L)
//    sleep(10000)
//    delConcreteClient(532125159977385984L)
//    sleep(10000)
//    addConcreteClient(532125159977385984L)

//    val schedule1 = sendSystemMailRegularly
//    val schedule2 = addClientRegularly
//    val schedule3 = deleteMailRegularly
//    system.terminate()
  }

  class ServerActor(clients: mutable.Map[Long, ActorRef]) extends Actor {

    val serverGUI = new ServerGUI(self)
    serverGUI.main(Array())

    def log(content: String): Unit = {
      serverGUI.log(content)
    }

    override def receive: Receive = {
      // 为GUI提供日志
      case Log(content) =>
        log(content)

      // 添加客户端，无法防止重复登陆
      case RequestAddPlayer(playerId, client) =>
        log(s"system: 添加客户端 ${client.path.name}")
        findPlayer(playerId) match {
          case Some(playerId) =>
            if (clients.contains(playerId)) {
              log(s"system: 玩家 $playerId 已经在线")
              client ! ReceiveReport("用户已在线")
            } else {
              clients += (playerId -> client)
              client ! ReceiveReport("连接成功")
              client ! RequestLoadMails(playerId)
            }
          case None =>
            client ! ReceiveReport("用户不存在")
        }

      // 删除客户端，先删用户列表，防止定时任务执行时发现用户信息不在缓存中再次加载，导致用户再次登陆时无法更新邮箱
      case RequestDelPlayer(playerId) =>
        log(s"system: 删除客户端 ${clients(playerId).path.name}")
        clients -= playerId
        deleteClient(playerId)

      // 收到玩家发送邮件的请求，将邮件存入数据库，判断是否成功(暂未实现),向发送方添加一封已发送邮件，如果接收方在线，直接发送邮件,不在线存入数据库
      case RequestSendMail(senderId, receiverId, mail) =>
        log(s"system: 玩家 $senderId 发送邮件给玩家 $receiverId")
        addPersonalMail(senderId, receiverId, mail) match {
          case Left(msg) =>
            log(s"system: $msg")
            sender() ! ReceiveReport(msg)
          case Right(mail) =>
            log(s"system: 玩家 $senderId 发送邮件成功")
            clients(senderId) ! ReceiveMails(List(mail))
            clients.get(receiverId) match {
              case Some(receiver) =>
                log(s"system: 玩家 $receiverId 在线，直接发送邮件")
                receiver ! ReceiveMails(List(mail))
              case None =>
                log(s"system: 玩家 $receiverId 不在线，邮件已存入数据库")
            }
        }

      // 发送系统邮件给所有在线玩家，将邮件存入数据库和删除缓存
      case SendSystemMail(mail) =>
        log("system: 发送系统邮件给所有在线玩家")
        val systemMail = addSystemMail(mail)
        clients.foreach{ client =>
          systemMail match {
            case Left(msg) =>
              log(s"system: $msg")
              sender() ! ReceiveReport(msg)
            case Right(mail) =>
              log(s"system: 发送系统邮件成功")
              client._2 ! ReceiveMails(List(mail))
          }
        }

      // 加载玩家邮件
      case RequestLoadMails(playerId) =>
        log(s"system: 玩家 $playerId 开始请求更新邮箱")
        loadPlayersMail(playerId) match {
          case Left(msg) =>
            log(s"system: $msg")
            sender() ! ReceiveReport(msg)
          case Right(mails) =>
            log(s"system: 玩家 $playerId 邮箱更新成功")
            sender() ! ReceiveMails(mails)
        }

      // 处理玩家阅读邮件请求，不需要等数据库和缓存更新，玩家本地直接更新成已读
      case RequestReadMail(playerId, mailId) =>
        log(s"system: 玩家 $playerId 开始阅读邮件")
        readMail(playerId, mailId)

      // 处理玩家领取邮件附件请求,数据库更新成功后，删除redis中的领取状态，玩家再更新邮箱
      case RequestCollectAttachment(playerId, mailId) =>
        log(s"system: 玩家 $playerId 开始领取邮件 $mailId 的附件")
        collectAttachment(playerId, mailId) match {
          case Left(msg) =>
            sender() ! ReceiveReport(msg)
          case Right(attachment) =>
            log(s"system: 玩家 $playerId 领取邮件 $mailId 的附件成功")
            sender() ! ReceiveObtainItems(attachment)
            sender() ! ReceiveCollectSuccess(mailId)
        }


      // 处理玩家删除邮件请求
      case RequestDelMail(playerId, mailId) =>
        log(s"system: 玩家 $playerId 开始删除邮件 $mailId")
        deleteMail(playerId, mailId) match {
          case Left(msg) =>
            sender() ! ReceiveReport(msg)
          case Right(_) =>
            sender() ! ReceiveDeleteSuccess(mailId)
        }

      case _ =>
        log(s"system: 服务端收到未知消息")
    }
  }

  class ClientActor(val playerId: Long,
                    val server: ActorRef,
                    val systemMails: mutable.Map[Long, SystemMail],
                    val personalMails: mutable.Map[Long, PersonalMail],
                    val sendMails: mutable.Map[Long, PersonalMail]) extends Actor {

    val clientGUI = new ClientGUI(self, playerId, systemMails, personalMails, sendMails)
    clientGUI.main(Array())

    private def refreshGUI(): Unit = {
      clientGUI.refreshGUI()
    }

    def log(content: String): Unit = {
      clientGUI.log(content)
    }

    private def addMail(mail: Mail): Unit = {
      mail match {
        case personalMail: PersonalMail if personalMail.receiverId == playerId =>
          log(s"player: 玩家 $playerId 收到一封个人邮件 ${personalMail.getMailId}")
          personalMails += (personalMail.getMailId -> personalMail)
        case personalMail: PersonalMail if personalMail.senderId == playerId =>
          log(s"player: 玩家 $playerId 发送了一封邮件 ${personalMail.getMailId}")
          sendMails += (personalMail.getMailId -> personalMail)
        case systemMail: SystemMail =>
          log(s"player: 玩家 $playerId 收到一封系统邮件 ${systemMail.getMailId}")
          systemMails += (systemMail.getMailId -> systemMail)
      }
    }

    override def receive: Receive = {
      // 为GUI提供日志
      case Log(content) =>
        log(content)

      // 向服务端发送连接请求
      case RequestAddPlayer(playerId, client) =>
        log(s"player: 玩家 $playerId 请求连接")
        server ! RequestAddPlayer(playerId, client)

      // 从服务端断开连接
      case RequestDelPlayer(playerId) =>
        log(s"player: 玩家 $playerId 请求断开连接")
        server ! RequestDelPlayer(playerId)
        context.stop(self)

      // 一般发生在玩家登陆时，加载邮件到本地
      case RequestLoadMails(playerId) =>
        log(s"player: 玩家 $playerId 请求拉取邮箱到本地")
        server ! RequestLoadMails(playerId)

      // 发送邮件
      case RequestSendMail(sender, receiver, mail) =>
        log(s"player: 玩家 $sender 请求发送邮件给玩家 $receiver")
        server ! RequestSendMail(playerId, receiver, mail)

      // 玩家收到邮件时，将邮件存入本地
      case ReceiveMails(mails) =>
        mails.foreach(addMail)
        refreshGUI()

      // 阅读邮件，直接更新本地的阅读状态
      case RequestReadMail(playerId, mailId) =>
        log(s"player: 玩家 $playerId 请求阅读邮件 $mailId")
        server ! RequestReadMail(playerId, mailId)
        if (personalMails.contains(mailId)) personalMails(mailId).setRead(true)
        else if (sendMails.contains(mailId)) sendMails(mailId).setRead(true)
        else if (systemMails.contains(mailId)) systemMails(mailId).setRead(true)
        refreshGUI()

      // 领取邮件附件
      case RequestCollectAttachment(playerId, mailId) =>
        log(s"player: 玩家 $playerId 请求获取邮件 $mailId 的附件")
        server ! RequestCollectAttachment(playerId, mailId)

      // 删除邮件
      case RequestDelMail(playerId, mailId) =>
        log(s"player: 玩家 $playerId 请求删除邮件 $mailId")
        server ! RequestDelMail(playerId, mailId)

      // 打印，貌似没用了？
      case ReceiveReport(msg) =>
        log(s"player: 玩家 $playerId 收到服务器发来的信息 $msg")

      // 领取附件成功,打印附件信息
      case ReceiveObtainItems(items) =>
        log(s"player: 玩家 $playerId 领取附件成功")
        items.foreach { case (item, quantity) =>
          log(s"player: 玩家 $playerId 领取了 ${item.getName} * $quantity")
        }

      // 领取附件成功，更新本地领取状态
      case ReceiveCollectSuccess(mailId) =>
        log(s"player: 玩家 $playerId 领取邮件 $mailId 的附件成功")
        if (personalMails.contains(mailId)) personalMails(mailId).setCollect(true)
        else if (sendMails.contains(mailId)) sendMails(mailId).setCollect(true)
        else if (systemMails.contains(mailId)) systemMails(mailId).setCollect(true)
        //        writeMailInFile(systemMails, personalMails, sendMails, playerId)
        refreshGUI()

      // 删除邮件成功，更新本地邮件
      case ReceiveDeleteSuccess(mailId) =>
        log(s"player: 玩家 $playerId 删除邮件 $mailId 成功")
        if (personalMails.contains(mailId)) personalMails.remove(mailId)
        else if (sendMails.contains(mailId)) sendMails.remove(mailId)
        else if (systemMails.contains(mailId)) systemMails.remove(mailId)
        //        writeMailInFile(systemMails, personalMails, sendMails, playerId)
        refreshGUI()

      case _ =>
        log(s"player: 玩家 $playerId 收到未知消息")
    }
  }

//  private def writeMailInFile(systemMails: mutable.Map[Long, SystemMail], personalMails: mutable.Map[Long, PersonalMail], sendMails: mutable.Map[Long, PersonalMail], playerId: Long): Unit = {
//    val fos = new java.io.FileOutputStream(s"mails-$playerId.txt")
//    fos.write("系统邮件\n".getBytes)
//    systemMails.foreach { case (_, mail) =>
//      fos.write(mail.toString.getBytes)
//      fos.write("\n".getBytes)
//    }
//    fos.write("收到的邮件\n".getBytes)
//    personalMails.foreach { case (_, mail) =>
//      fos.write(mail.toString.getBytes)
//      fos.write("\n".getBytes)
//    }
//    fos.write("发送的邮件\n".getBytes)
//    sendMails.foreach { case (_, mail) =>
//      fos.write(mail.toString.getBytes)
//      fos.write("\n".getBytes)
//    }
//    fos.close()
//  }

  private def findPlayer(playerId: Long): Option[Long] = {
    PlayerService.allPlayers().find(_.getPlayerId == playerId).map(_.getPlayerId)
  }

  // 删除客户端
  private def deleteClient(playerId: Long): Unit = {
    JedisHelper.execute{ jedis =>
      jedis.del(playerId2Key(playerId))
    }
  }

  // 将邮件写入文件
  private def writeMailInFile(mails: List[Mail], playerId: Long): Unit = {
    val fos = new java.io.FileOutputStream(s"mails-$playerId.txt")
    mails.foreach { mail =>
      fos.write(mail.toString.getBytes)
      fos.write("\n".getBytes)
    }
    fos.close()
  }

  // 与数据库进行同步，将数据库中的邮件加载到用户内存中
  private def loadPlayersMail(playerId: Long): Either[String, List[Mail]] = {
    JedisHelper.execute { jedis =>
      if (jedis.hexists(playerId2Key(playerId), keyForMailsRead) && jedis.hexists(playerId2Key(playerId), keyForMailsCollect)) {
        myLog(s"process: 玩家 $playerId 的邮箱已经加载到内存中")
        Left("邮件已经被加载到内存中")
      } else if (jedis.hexists(playerId2Key(playerId), keyForMailsRead) && !jedis.hexists(playerId2Key(playerId), keyForMailsCollect)) {
        myLog(s"process: 玩家 $playerId 的邮箱已经加载到内存中，但是领取状态更新后被删除")
        val mailsCollect = PlayerService.getCollectStatus(playerId)
        jedis.hset(playerId2Key(playerId), keyForMailsCollect, mailsCollect)
        Left("领取状态已更新")
      } else {
        myLog(s"process: 玩家 $playerId 邮箱未加载到内存中")
        val mails = MailService.personalMails(playerId)
        mails ++= loadSystemMails()
        mails.toList match {
          case Nil => Left("用户没有邮件")
          case mails =>
            myLog(s"process: 玩家 $playerId 邮箱加载成功")
            // 讲邮件已读已领取状态存入redis，将邮件存入用户内存
            val player = PlayerService.getPlayer(playerId)
            val mailsRead = player.getMailsRead
            val mailsCollect = player.getMailsCollect
            jedis.hset(playerId2Key(playerId), keyForMailsCollect, mailsCollect)
            jedis.hset(playerId2Key(playerId), keyForMailsRead, mailsRead)
            if (mailsRead.nonEmpty) {
              mailsRead.split(",").foreach { mailId =>
                mails.find(_.getMailId == mailId.toLong).foreach(_.setRead(true))
              }
            }
            if (mailsCollect.nonEmpty) {
              mailsCollect.split(",").foreach { mailId =>
                mails.find(_.getMailId == mailId.toLong).foreach(_.setCollect(true))
              }
            }
            Right(mails)
        }
      }
    }
  }

  // 增加个人邮件，收件人邮件数加一，使用分布式锁防止收件人同时收到多封邮件出现计数错误
  private def addPersonalMail(senderId: Long, receiverId: Long, mail: PersonalMail): Either[String, Mail] = {
    try {
      Right(MailService.addPersonalMail(senderId, receiverId, mail))
    } catch {
      case e: SQLException if e.getSQLState == MyGlobalConfig.SQLERRORMAILCOUNTEXCEED =>
        myLog(s"process: 玩家 $receiverId 邮箱已满")
        Left("收件人邮箱已满发送失败")
      case e: IllegalArgumentException =>
        myLog(s"process: 玩家 $receiverId 邮件发送失败, 错误信息：${e.getMessage}")
        Left("邮件发送失败, 错误信息：" + e.getMessage)
      case e: SQLIntegrityConstraintViolationException =>
        myLog(s"process: 玩家 $receiverId 邮件发送失败, 用户不存在, 错误信息：${e.getMessage}")
        Left("邮件发送失败, 用户不存在, 错误信息：" + e.getMessage)
      case e: Exception =>
        myLog(s"process: 玩家 $receiverId 邮件发送失败， 未捕获异常")
        Left("邮件发送失败， 未捕获异常")
    }
  }

  // 将系统邮件存入数据库，并删除缓存
  private def addSystemMail(mail: SystemMail): Either[String, SystemMail] = {
    try {
      var systemMail: SystemMail = null
      JedisHelper.execute { jedis =>
        systemMail = MailService.addSystemMail(mail)
        jedis.del(keyForSystemMail)
      }
      Right(systemMail)
    } catch {
      case e: IllegalArgumentException =>
        myLog(s"process: 系统邮件发送失败, 错误信息：${e.getMessage}")
        Left("系统邮件发送失败, 错误信息：" + e.getMessage)
      case e: Exception =>
        myLog(s"process: 系统邮件发送失败， 未捕获异常")
        Left("系统邮件发送失败， 未捕获异常")
    }
  }

  // 加载系统邮件, 先从redis中加载，如果没有再从数据库中加载
  private def loadSystemMails(): mutable.ListBuffer[Mail] = {
    JedisHelper.execute { jedis =>
      val systemMails = mutable.ListBuffer[Mail]()
      val mails = jedis.hgetAll(keyForSystemMail)
      if (mails.isEmpty) {
        myLog("process: 系统邮件未加载到缓存中, 开始加载")
        systemMails ++= MailService.systemMails()
        systemMails.foreach { mail =>
          jedis.hset(keyForSystemMail, systemMailId2Key(mail.getMailId), mail.toString)
        }
      } else {
        myLog("process: 系统邮件已加载到缓存中")
        mails.foreach { case (mailId, mail) =>
          systemMails += MapBeanUtils.json2SystemMail(mail)
        }
      }
      systemMails
    }
  }

  /*
   * 用户读取邮件，在redis中记录邮件已读状态，定时任务将状态同步到数据库
   * 读取不存在的内存时可能会反复加载，需要修改，可以设置循环次数
   */
  private def readMail(playerId: Long, mailId: Long): Unit = {
    JedisHelper.execute { jedis =>
      jedis.hget(playerId2Key(playerId), keyForMailsRead) match {
        case null =>
          myLog(s"process: 玩家 $playerId 的邮件可能未加载到缓存中, 尝试再次加载")
          loadPlayersMail(playerId)
          readMail(playerId, mailId)
        case mailsRead =>
          if (mailsRead.contains(mailId.toString)) myLog(s"玩家 $playerId 已经阅读过邮件 $mailId") else {
            myLog(s"process: 玩家 $playerId 阅读了邮件 $mailId")
            jedis.hset(playerId2Key(playerId), keyForMailsRead, s"$mailsRead$mailId,")
          }
      }
    }
  }

  /**
   * 用户领取附件，立刻同步到数据库。 领取附件时需要加锁，防止多个用户同时领取同一封邮件的附件
   * 附件领取成功后，将领取状态从redis中删除,保持一致性
   */
  private def collectAttachment(playerId: Long, mailId: Long): Either[String, Map[Item, Int]] = {
    val mail = MailService.getMail(mailId)
    val attachmentJson = mail.getAttachment
    if (attachmentJson == "{}") return Left("邮件没有附件")
    val deadline = mail.getDeadline
    if (deadline.isBefore(LocalDateTime.now)) return Left("邮件已过期")
    val lockKey = distributionKeyForAttachmentCollect(playerId, mailId)
    val lockValue = System.currentTimeMillis().toString

    try {
      JedisHelper.executeWithDistributionLock(lockKey, lockValue, 20) { jedis =>
        val attachment = MapBean.toMutableMap(attachmentJson).toMap.asInstanceOf[Map[String, Int]]
        PlayerService.collectAttachment(playerId, mailId, attachment)
        jedis.hdel(playerId2Key(playerId), keyForMailsCollect)
        Right(attachment.map { case (itemId, quantity) => (ItemService.getItem(itemId.toLong), quantity) })
      }
    } catch {
      case e: IllegalStateException =>
        Left(e.toString)
      case e: Exception =>
        Left("领取附件失败")
    }
  }

  /*
   * 删除邮件，需要注意防止定时同步任务把缓存中邮件阅读状态同步到数据库，导致删除失败
   * 目前没有想到好的解决方案，只能在删除和同步时加上一样的锁，保证同步任务不会导致被删除的邮件再次被同步
   */
  private def deleteMail(playerId: Long, mailId: Long): Either[String, Unit] = {
    try {
      MailService.getMail(mailId) match {
        case mail: SystemMail =>
          myLog(s"process: 玩家尝试 $playerId 删除系统邮件 $mailId")
          JedisHelper.executeWithDistributionLock(distributionKeyForStatusChange, System.currentTimeMillis().toString, 20) { jedis =>
            jedis.hdel(playerId2Key(playerId), keyForMailsRead)
            Right(MailService.deleteSystemMail(playerId, mailId))
          }
        case mail: PersonalMail if mail.senderId == playerId =>
          myLog(s"process: 玩家尝试 $playerId 删除发送的邮件 $mailId")
          Right(MailService.deleteMailSend(playerId, mailId))
        case mail: PersonalMail if mail.receiverId == playerId =>
          myLog(s"process: 玩家尝试 $playerId 删除收到的邮件 $mailId")
          JedisHelper.executeWithDistributionLock(distributionKeyForStatusChange, System.currentTimeMillis().toString, 20) { jedis =>
            jedis.hdel(playerId2Key(playerId), keyForMailsRead)
            Right(MailService.deleteMailReceive(playerId, mailId))
          }
        case _ =>
          myLog(s"process: 玩家 $playerId 删除邮件 $mailId 失败, 删除不合法")
          Left("删除邮件失败")
      }
    } catch {
      case e: Exception =>
        Left("删除邮件失败")
    }
  }

  // 定时任务，同步邮件已读状态到数据库，当尝试加载不存在的内存时，会反复加载，需要修改，可以设置循环次数
  def syncMailsRead(): Unit = {
    val key = distributionKeyForStatusChange
    val value = System.currentTimeMillis().toString

    myLog("process: 同步邮件已读状态")
    JedisHelper.executeWithDistributionLock(key, value, 20) { jedis =>
      val players: Iterable[Long] = jedis.keys("*").toArray.flatMap { key => key2PlayerId(key.asInstanceOf[String]) }
      players.foreach { playerId =>
        jedis.hget(playerId2Key(playerId), keyForMailsRead) match {
          case null =>
            myLog(s"process: 玩家 $playerId 的邮件已读状态未加载到缓存中")
            loadPlayersMail(playerId)
            syncMailsRead()
          case mailsRead =>
            if (mailsRead.isEmpty) {
              myLog(s"process: 玩家 $playerId 的邮件已读状态为空")
            } else {
              myLog(s"process: 玩家 $playerId 的邮件已读状态同步到数据库")
              PlayerService.updateMailsRead(playerId, mailsRead)
            }
        }
      }
    }
  }
}
