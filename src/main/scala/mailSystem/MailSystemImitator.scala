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
    def randomPlayerId = MailSystemInvoker.randomPlayerId

    val fixedPlayer = PlayerService.getPlayer(randomPlayerId)
    val fixedMails = MailService.mails(fixedPlayer.getPlayerId)

    def randomMailId = MailSystemInvoker.getRandom(fixedMails).getMailId

    val fixedMailId = randomMailId

    val system = ActorSystem("MyMailSystem")

    val serverActor = system.actorOf(Props(new ServerActor(mutable.Map[Long, ActorRef]())), "serverActor")
    myLog("新增服务端" + serverActor.path)

    for (i <- 1 to 20) {
      val playerId = randomPlayerId
      val newClient = new ClientActor(playerId, serverActor, mutable.Map[Long, SystemMail](), mutable.Map[Long, PersonalMail](), mutable.Map[Long, PersonalMail]())
      val clientActor = system.actorOf(Props(newClient), s"clientActor-$playerId-$i")
      myLog("新增客户端" + clientActor.path)
      serverActor ! AddClient(playerId, clientActor)
    }

    val scheduler = system.scheduler
//    val cancellable = scheduler.scheduleWithFixedDelay(0.seconds, 10.seconds)(() => syncMailsRead())
//
//    val cancellable2 = scheduler.scheduleWithFixedDelay(0.seconds, 2.seconds)(() => clientActor ! ReadMail(randomPlayerId, randomMailId))

//    val cancellable3 = scheduler.scheduleWithFixedDelay(0.seconds, 5.seconds)(() => clientActor ! CollectAttachment(randomPlayerId, randomMailId))

//    val cancellable4 = scheduler.scheduleWithFixedDelay(0.seconds, 5.seconds)(() => {
//      val sender = randomPlayerId
//      val receiver = randomPlayerId
//      serverActor ! SendPersonalMail(sender , receiver, new PersonalMail(sender, receiver, "title", "content", "{}", "{}"))
//    })

    //    system.terminate()
  }

  case class AddClient(playerId: Long, client: ActorRef)

  case class SendPersonalMail(sender: Long, receiver: Long, mail: PersonalMail)

  case class SendSystemMail(mail: SystemMail)

  case class LoadPlayersMail(playerId: Long)

  case class ReadMail(playerId: Long, mailId: Long)

  case class CollectAttachment(playerId: Long, mailId: Long)

  case class DelMail(playerId: Long, mailId: Long)

  case class ReportError(msg: String)

  case class ReceiveMails(mails: List[Mail])

  class ServerActor(clients: mutable.Map[Long, ActorRef]) extends Actor {
    override def receive: Receive = {
      case AddClient(playerId, client) =>
        myLog(s"system: 添加客户端 ${client.path.name}")
        clients += (playerId -> client)
        client ! LoadPlayersMail(playerId)

      case SendPersonalMail(senderId, receiverId, mail) =>
        myLog(s"system: 玩家 $senderId 发送邮件给玩家 $receiverId")
        MailService.addPersonalMail(mail)
        sender() ! ReceiveMails(List(mail))
        clients.get(receiverId) match {
          case Some(receiver) =>
            myLog(s"system: 玩家 $receiverId 在线，直接发送邮件")
            receiver ! ReceiveMails(List(mail))
          case None =>
            myLog(s"system: 玩家 $receiverId 不在线，邮件已存入数据库")
        }

      case SendSystemMail(mail) =>
        myLog("system: 发送系统邮件给所有在线玩家")
        MailService.addSystemMail(mail)
        clients.foreach(_._2 ! ReceiveMails(List(mail)))

      case LoadPlayersMail(playerId) =>
        myLog(s"system: 玩家 $playerId 开始请求更新邮箱")
        loadPlayersMail(playerId) match {
          case Left(msg) =>
            myLog(s"system: $msg")
            sender() ! ReportError(msg)
          case Right(mails) =>
            myLog(s"system: 玩家 $playerId 邮箱更新成功")
            sender() ! ReceiveMails(mails)
        }

      case ReadMail(playerId, mailId) =>
        myLog(s"system: 玩家 $playerId 开始阅读邮件")
        readMail(playerId, mailId)

      case CollectAttachment(playerId, mailId) =>
        myLog(s"system: 玩家 $playerId 开始领取邮件 $mailId 的附件")
        collectAttachment(playerId, mailId)

      case DelMail(playerId, mailId) =>
        myLog(s"system: 玩家 $playerId 开始删除邮件 $mailId")
        PlayerService.deleteMail(playerId, mailId)

      case _ =>
        myLog(s"system: 服务端收到未知消息")
    }
  }

  class ClientActor(val playerId: Long,
                    val server: ActorRef,
                    val systemMails: mutable.Map[Long, SystemMail],
                    val personalMails: mutable.Map[Long, PersonalMail],
                    val sendMails: mutable.Map[Long, PersonalMail]) extends Actor {
    override def receive: Receive = {
      case SendPersonalMail(sender, receiver, mail) =>
        myLog(s"player: 玩家 $sender 发送邮件给玩家 $receiver")
        server ! SendPersonalMail(sender, receiver, mail)

      case LoadPlayersMail(playerId) =>
        myLog(s"player: 玩家 $playerId 请求拉取邮箱到本地")
        server ! LoadPlayersMail(playerId)

      case ReceiveMails(mails) =>
        myLog(s"player: 玩家 $playerId 收到邮件")
        mails.foreach {
            case personalMail: PersonalMail if personalMail.receiverId == playerId=>
              personalMails += (personalMail.getMailId -> personalMail)
            case personalMail: PersonalMail if personalMail.senderId == playerId =>
              sendMails += (personalMail.getMailId -> personalMail)
            case systemMail: SystemMail =>
              systemMails += (systemMail.getMailId -> systemMail)
          }

      case ReadMail(playerId, mailId) =>
        myLog(s"player: 玩家 $playerId 请求阅读邮件 $mailId")
        server ! ReadMail(playerId, mailId)

      case CollectAttachment(playerId, mailId) =>
        myLog(s"player: 玩家 $playerId 请求获取邮件 $mailId 的附件")
        server ! CollectAttachment(playerId, mailId)

      case DelMail(playerId, mailId) =>
        myLog(s"player: 玩家 $playerId 请求删除邮件 $mailId")
        server ! DelMail(playerId, mailId)

      case ReportError(msg) =>
        myLog(s"player: $msg")

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
        myLog(s"玩家 $playerId 的邮箱已经加载到内存中，但是领取状态更新后被删除")
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

  /*
   * 用户读取邮件，在redis中记录邮件已读状态，定时任务将状态同步到数据库
   * 读取不存在的内存时可能会反复加载，需要修改
   */
  def readMail(playerId: Long, mailId: Long): Unit = {
    JedisHelper.execute { jedis =>
      jedis.hget(getPlayersKey(playerId), KEY_MAIL_READ) match {
        case null =>
          myLog(s"玩家 $playerId 的邮件可能未加载到缓存中, 尝试再次加载")
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

  /**
   * 用户领取附件，立刻同步到数据库。 领取附件时需要加锁，防止多个用户同时领取同一封邮件的附件
   * 附件领取成功后，将领取状态从redis中删除,保持一致性
   */
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
