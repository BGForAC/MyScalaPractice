package mailSystem

import akka.actor._
import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.service.{MailService, PlayerService}
import mailSystem.utils.JedisHelper

import scala.collection.mutable

object MailSystemImitator {

  private def myLog(s: String): Unit = {
    println(s)
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("MyMailSystem")
    val serverActor = system.actorOf(Props(new ServerActor(mutable.Map[Long, ActorRef]())), "serverActor")
    myLog("新增服务端" + serverActor.path)

    val playerId = MailSystemInvoker.randomPlayerId
    val player = PlayerService.getPlayer(playerId)
    val mails = MailService.mails(playerId)
    val clientActor = system.actorOf(Props(new ClientActor(playerId, serverActor)), s"clientActor-$playerId")
    myLog("新增客户端" + clientActor.path)
    serverActor ! AddClient(playerId, clientActor)
    clientActor ! LoadPlayersMail(playerId)

    val mailId = MailSystemInvoker.getRandom(mails).getMailId

    clientActor ! ReadMail(playerId, mailId)
    clientActor ! ReadMail(playerId, mailId)
    clientActor ! ReadMail(playerId, mailId)
    clientActor ! ReadMail(playerId, mailId)
    clientActor ! ReadMail(playerId, mailId)
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
  case class PlayersMail(playerId: Long, mails: List[Mail])
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
        myLog(s"system: 玩家 $playerId 请求拉取邮箱")
        loadPlayersMail(playerId) match {
          case Left(msg) =>
            myLog(s"system: $msg")
          case Right(mails) =>
            myLog(s"system: 玩家 $playerId 邮箱加载成功")
            sender() ! PlayersMail(playerId, mails)
        }
      case ReadMail(playerId, mailId) =>
        myLog("system: 玩家阅读邮件")
        readMail(playerId, mailId)
      case _ =>
        myLog("system: 服务端收到未知消息")
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
        myLog(s"player: 玩家请求拉取邮箱到本地")
        server ! LoadPlayersMail(playerId)
      case PlayersMail(playerId, mails) =>
        myLog(s"player: 打印玩家 $playerId 的本地邮箱")
        writeMailInFile(mails, playerId)
      case ReadMail(playerId, mailId) =>
        myLog(s"player: 玩家 $playerId 阅读邮件 $mailId")
        server ! ReadMail(playerId, mailId)
      case _ =>
        myLog("player: 客户端收到未知消息")
    }
  }

  //将邮件写入文件
  def writeMailInFile(mails: List[Mail], playerId: Long): Unit = {
    val fos = new java.io.FileOutputStream(s"mails-$playerId.txt")
    mails.foreach{ mail =>
      fos.write(mail.toString.getBytes)
      fos.write("\n".getBytes)
    }
    fos.close()
  }

  //与数据库进行同步，将数据库中的邮件加载到用户内存中
  def loadPlayersMail(playerId: Long): Either[String, List[Mail]] = {
    val jedis = JedisHelper.getJedis
    try {
      if (jedis.hexists(playerId.toString, "mails_read")) {
        myLog(s"玩家 $playerId 的邮箱已经加载到内存中")
        Left("邮件已经被加载到内存中")
      } else {
        myLog(s"玩家 $playerId 邮箱未加载到内存中")
        val mails = MailService.mails(playerId)
        mails.toList match {
          case Nil => Left("用户没有邮件")
          case mails =>
            myLog(s"玩家 $playerId 邮箱加载成功")
            //讲邮件已读未读状态存入redis，将邮件存入用户内存
            val player = PlayerService.getPlayer(playerId)
            jedis.hset(playerId.toString, "mails_collect", player.getMailsCollect)
            jedis.hset(playerId.toString, "mails_read", player.getMailsRead)
            Right(mails)
        }
      }
    } finally {
      JedisHelper.closeJedis(jedis)
    }
  }

  //用户读取邮件，在redis中记录邮件已读状态，定时任务将状态同步到数据库
  def readMail(playerId: Long, mailId: Long): Unit = {
    val jedis = JedisHelper.getJedis
    try {
      jedis.hget(playerId.toString, "mails_read") match {
        case null => throw new Exception(s"玩家 $playerId 的邮箱未加载到内存中")
        case mailsRead =>
          if (mailsRead.contains(mailId.toString)) myLog(s"玩家 $playerId 已经阅读过邮件 $mailId") else {
            myLog(s"玩家 $playerId 阅读了邮件 $mailId")
            jedis.hset(playerId.toString, "mails_read", s"$mailsRead,$mailId")
          }
      }
    } finally {
      JedisHelper.closeJedis(jedis)
    }
  }

  //将用户的邮件领取情况与数据库同步
  def loadCollectStatus(playerId: Long, mailId: Long): Unit = {
    PlayerService.get
  }

  //用户领取附件，在redis中记录邮件已领取状态，立刻同步到数据库
  def collectAttachment(playerId: Long, mailId: Long): Unit = {
    PlayerService.collectAttachment()
    val jedis = JedisHelper.getJedis
    try {
      jedis.hget(playerId.toString, "mails_collect") match {
        case null =>
          loadCollectStatus(playerId, mailId)
        case mailsCollect =>
          if (mailsCollect.contains(mailId.toString)) myLog(s"玩家 $playerId 已经领取过邮件 $mailId 的附件") else {
            myLog(s"玩家 $playerId 领取了邮件 $mailId 的附件")
          }
      }
    } finally {
      JedisHelper.closeJedis(jedis)
    }
  }
}
