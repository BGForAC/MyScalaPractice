package mailSystem

import akka.actor._
import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.service.{MailService, PlayerService}
import mailSystem.utils.{JedisHelper, MapBeanUtils}

import scala.collection.convert.ImplicitConversions.`map AsScala`
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

object MailSystemImitator {

  def getPlayersKey(playerId: Long): String = {
    s"player:$playerId"
  }

  def key2PlayersId(key: String): Option[Long] = {
    key match {
      case key if key.startsWith("player:") => Some(key.split(":")(1).toLong)
      case _ => None
    }
  }

  def getSystemMailsKey(systemMailId: Long): String = {
    s"system_mail:$systemMailId"
  }

  def getDistributionKeyForAttachmentCollect(playerId: Long, mailId: Long): String = {
    s"distribution:attachment:collect:$playerId:$mailId"
  }

  def getDistributionKeyForLoadSystemMail(): String = {
    "distribution:load:system:mail"
  }

  private final val KEY_SYSTEM_MAIL = "system_mail"
  private final val KEY_MAIL_READ = "mails_read"
  private final val KEY_MAIL_COLLECT = "mails_collect"

  private def myLog(content: String): Unit = {
    println(content)
  }

  def main(args: Array[String]): Unit = {
    def playerId = MailSystemInvoker.randomPlayerId

    val player = PlayerService.getPlayer(playerId)
    val mails = MailService.mails(playerId)

    def randomMailId = MailSystemInvoker.getRandom(mails).getMailId

    val mailId = randomMailId

    val system = ActorSystem("MyMailSystem")

    val serverActor = system.actorOf(Props(new ServerActor(mutable.Map[Long, ActorRef]())), "serverActor")
    myLog("新增服务端" + serverActor.path)

    val clientActor = system.actorOf(Props(new ClientActor(playerId, serverActor)), s"clientActor-$playerId")
    myLog("新增客户端" + clientActor.path)
    serverActor ! AddClient(playerId, clientActor)

    val clientActor2 = system.actorOf(Props(new ClientActor(playerId, serverActor)), s"clientActor-$playerId")
    myLog("新增客户端" + clientActor2.path)
    serverActor ! AddClient(playerId, clientActor2)

    val scheduler = system.scheduler
    val cancellable = scheduler.scheduleWithFixedDelay(0.seconds, 10.seconds)(() => syncMailsRead())

    val cancellable2 = scheduler.scheduleWithFixedDelay(0.seconds, 2.seconds)(() => clientActor ! ReadMail(playerId, randomMailId))

//    val cancellable3 = scheduler.scheduleWithFixedDelay(0.seconds, 5.seconds)(() => clientActor ! CollectAttachment(playerId, randomMailId))

    //        clientActor ! CollectAttachment(playerId, mailId)
    //    clientActor ! CollectAttachment(playerId, mailId)
    //    clientActor ! CollectAttachment(playerId, mailId)
    //    clientActor ! CollectAttachment(playerId, mailId)
    //    clientActor ! CollectAttachment(playerId, mailId)

    //    clientActor ! SendPersonalMail(playerId, playerId, new PersonalMail(playerId, playerId, "title", "content", "attachment"))
    //    serverActor ! SendSystemMail(new SystemMail("title", "content", "attachment", "filter"))

    //    system.terminate()
  }

  case class AddClient(playerId: Long, client: ActorRef)

  case class SendPersonalMail(sender: Long, receiver: Long, mail: PersonalMail)

  case class SendSystemMail(mail: SystemMail)

  case class ReceivePersonalMail(mail: PersonalMail)

  case class ReceiveSystemMail(mail: SystemMail)

  case class LoadPlayersMail(playerId: Long)

  case class PrintPlayersMail(playerId: Long, mails: List[Mail])

  case class ReadMail(playerId: Long, mailId: Long)

  case class CollectAttachment(playerId: Long, mailId: Long)

  class ServerActor(clients: mutable.Map[Long, ActorRef]) extends Actor {
    override def receive: Receive = {
      case AddClient(playerId, client) =>
        myLog(s"system: 添加客户端 ${client.path.name}")
        clients += (playerId -> client)
        client ! LoadPlayersMail(playerId)

      case SendPersonalMail(sender, receiver, mail) =>
        myLog(s"system: 玩家 $sender 发送邮件给玩家 $receiver")
        clients.get(receiver) match {
          case Some(client) =>
            myLog(s"system: 玩家 $receiver 在线，直接发送邮件")
            client ! ReceivePersonalMail(mail)
          case None =>
            myLog(s"system: 玩家 $receiver 不在线，邮件已存入数据库")
        }

      case SendSystemMail(mail) =>
        myLog("system: 发送系统邮件给所有玩家")
        clients.foreach(_._2 ! ReceiveSystemMail(mail))

      case LoadPlayersMail(playerId) =>
        myLog(s"system: 玩家 $playerId 开始请求拉取邮箱")
        loadPlayersMail(playerId) match {
          case Left(msg) =>
            myLog(s"system: $msg")
          case Right(mails) =>
            myLog(s"system: 玩家 $playerId 邮箱加载成功")
            sender() ! PrintPlayersMail(playerId, mails)
        }

      case ReadMail(playerId, mailId) =>
        myLog(s"system: 玩家 $playerId 开始阅读邮件")
        readMail(playerId, mailId)

      case CollectAttachment(playerId, mailId) =>
        myLog(s"system: 玩家 $playerId 开始领取邮件 $mailId 的附件")
        collectAttachment(playerId, mailId)

      case _ =>
        myLog(s"system: 服务端收到未知消息")
    }
  }

  class ClientActor(val playerId: Long, val server: ActorRef) extends Actor {
    override def receive: Receive = {
      case ReceivePersonalMail(mail) =>
        myLog(s"player: 玩家 $playerId 收到邮件" + mail)

      case ReceiveSystemMail(mail) =>
        myLog(s"player: 玩家 $playerId 收到邮件" + mail)

      case SendPersonalMail(sender, receiver, mail) =>
        myLog(s"player: 玩家 $sender 发送邮件给玩家 $receiver")
        server ! SendPersonalMail(sender, receiver, mail)

      case LoadPlayersMail(playerId) =>
        myLog(s"player: 玩家 $playerId 请求拉取邮箱到本地")
        server ! LoadPlayersMail(playerId)

      case PrintPlayersMail(playerId, mails) =>
        myLog(s"player: 打印玩家 $playerId 的本地邮箱")
        writeMailInFile(mails, playerId)

      case ReadMail(playerId, mailId) =>
        myLog(s"player: 玩家 $playerId 请求阅读邮件 $mailId")
        server ! ReadMail(playerId, mailId)

      case CollectAttachment(playerId, mailId) =>
        myLog(s"player: 玩家 $playerId 请求获取邮件 $mailId 的附件")
        server ! CollectAttachment(playerId, mailId)

      case _ =>
        myLog("player: 客户端收到未知消息")
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
  def loadPlayersMail(playerId: Long): Either[String, List[Mail]] = {
    JedisHelper.execute { jedis =>
      if (jedis.hexists(getPlayersKey(playerId), KEY_MAIL_READ) && jedis.hexists(getPlayersKey(playerId), KEY_MAIL_COLLECT)) {
        myLog(s"玩家 $playerId 的邮箱已经加载到内存中")
        Left("邮件已经被加载到内存中")
      } else if (jedis.hexists(getPlayersKey(playerId), KEY_MAIL_READ) && !jedis.hexists(getPlayersKey(playerId), KEY_MAIL_COLLECT)) {
        myLog(s"玩家 $playerId 的邮箱已经加载到内存中，但是领取状态未加载")
        val mailsCollect = PlayerService.getCollectStatus(playerId)
        jedis.hset(getPlayersKey(playerId), KEY_MAIL_COLLECT, mailsCollect)
        Left("领取状态已更新")
      } else {
        myLog(s"玩家 $playerId 邮箱未加载到内存中")
        val mails = MailService.personalMails(playerId)
        mails ++= loadSystemMails()
        mails.toList match {
          case Nil => Left("用户没有邮件")
          case mails =>
            myLog(s"玩家 $playerId 邮箱加载成功")
            // 讲邮件已读未读状态存入redis，将邮件存入用户内存
            val player = PlayerService.getPlayer(playerId)
            jedis.hset(getPlayersKey(playerId), KEY_MAIL_COLLECT, player.getMailsCollect)
            jedis.hset(getPlayersKey(playerId), KEY_MAIL_READ, player.getMailsRead)
            Right(mails)
        }
      }
    }
  }

  // 加载系统邮件, 先从redis中加载，如果没有再从数据库中加载
  private def loadSystemMails(): mutable.ListBuffer[Mail] = {
    val lockKey = getDistributionKeyForLoadSystemMail()
    val lockValue = System.currentTimeMillis().toString

    JedisHelper.executeWithDistributionLock(lockKey, lockValue, 20) { jedis =>
      val systemMails = mutable.ListBuffer[Mail]()
      val mails = jedis.hgetAll(KEY_SYSTEM_MAIL)
      if (mails.isEmpty) {
        myLog("系统邮件未加载到缓存中, 开始加载")
        systemMails ++= MailService.systemMails()
        systemMails.foreach { mail =>
          jedis.hset(KEY_SYSTEM_MAIL, getSystemMailsKey(mail.getMailId), mail.toString)
        }
      } else {
        myLog("系统邮件已加载到缓存中")
        mails.foreach { case (mailId, mail) =>
          //          System.out.println(mail)
          systemMails += MapBeanUtils.json2SystemMail(mail)
        }
      }
      systemMails
    }
  }

  // 用户读取邮件，在redis中记录邮件已读状态，定时任务将状态同步到数据库
  def readMail(playerId: Long, mailId: Long): Unit = {
    JedisHelper.execute { jedis =>
      jedis.hget(getPlayersKey(playerId), KEY_MAIL_READ) match {
        case null =>
          myLog(s"玩家 $playerId 的邮件未加载到内存中, 开始加载")
          loadPlayersMail(playerId)
          readMail(playerId, mailId)
        case mailsRead =>
          if (mailsRead.contains(mailId.toString)) myLog(s"玩家 $playerId 已经阅读过邮件 $mailId") else {
            myLog(s"玩家 $playerId 阅读了邮件 $mailId")
            jedis.hset(getPlayersKey(playerId), KEY_MAIL_READ, s"$mailsRead$mailId,")
          }
      }
    }
  }

  // 用户领取附件，在redis中记录邮件已领取状态，立刻同步到数据库
  def collectAttachment(playerId: Long, mailId: Long): Unit = {
    val attachmentJson = MailService.getAttachment(mailId)
    val lockKey = getDistributionKeyForAttachmentCollect(playerId, mailId)
    val lockValue = System.currentTimeMillis().toString

    JedisHelper.executeWithDistributionLock(lockKey, lockValue, 20) { jedis =>
      PlayerService.collectAttachment(playerId, mailId, attachmentJson)
      jedis.hdel(getPlayersKey(playerId), KEY_MAIL_COLLECT)
    }
  }

  def syncMailsRead(): Unit = {
    println("同步邮件已读状态")
    JedisHelper.execute { jedis =>
      val players: Iterable[Long] = jedis.keys("*").toArray.flatMap { key => key2PlayersId(key.asInstanceOf[String]) }
      players.foreach { playerId =>
        val mailsRead = jedis.hget(getPlayersKey(playerId), KEY_MAIL_READ)
        println(s"玩家 $playerId 的邮件已读状态：$mailsRead")
        if (mailsRead != null) PlayerService.updateMailsRead(playerId, mailsRead)
      }
    }
  }

}
