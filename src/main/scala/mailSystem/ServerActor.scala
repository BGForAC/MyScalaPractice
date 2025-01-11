package mailSystem

import akka.actor.{Actor, ActorRef, ActorSystem}
import mailSystem.Messages._
import MailSystemImpl._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

/**
 * 服务端Actor
 */
class ServerActor(clients: mutable.Map[Long, ActorRef], actorSystem: ActorSystem) extends Actor {

  private val serverGUI = new ServerGUI(self)
  serverGUI.main(Array())

  override def postStop(): Unit = serverGUI.quit()

  def log(content: String): Unit = {
    serverGUI.log(content)
  }

  actorSystem.scheduler.scheduleWithFixedDelay(0.seconds, 10.seconds)(() => syncMailsRead())

  override def receive: Receive = {
    // 为GUI提供日志
    case Log(content) =>
      log(content)

    // 添加客户端
    case RequestAddPlayer(playerId, client) =>
      log(s"system: 添加客户端 ${client.path.name}")
      findPlayer(playerId) match {
        case Some(playerId) =>
          if (clients.contains(playerId)) {
            log(s"system: 玩家 $playerId 已经在线")
            client ! ReceiveReport("用户已在线")
          } else {
            log(s"system: 玩家 $playerId 连接成功")
            clients += (playerId -> client)
            client ! ReceiveReport("连接成功")
            client ! RequestLoadMails(playerId)
          }
        case None =>
          client ! ReceiveReport("用户不存在")
      }

    // 删除客户端，先删用户列表，防止定时任务执行时发现用户信息不在缓存中再次加载，导致用户再次登陆时无法拉取邮箱
    case RequestDelPlayer(playerId) =>
      if (clients.contains(playerId)) {
        log(s"system: 删除客户端 ${clients(playerId).path.name}")
        clients -= playerId
        deleteClient(playerId)
      }

    // 收到玩家发送邮件的请求，将邮件存入数据库,向发送方添加一封已发送邮件，如果接收方在线，直接发送邮件,不在线存入数据库
    case RequestSendMail(senderId, receiverId, mail) =>
      log(s"system: 玩家 $senderId 发送邮件给玩家 $receiverId")
      addPersonalMail(senderId, receiverId, mail) match {
        case Left(msg) =>
          log(s"system: 玩家 $senderId 发送邮件失败")
          log(s"system: $msg")
          sender() ! ReceiveReport(msg)
        case Right(mail) =>
          log(s"system: 玩家 $senderId 发送邮件成功")
          def sendMessage(playerId: Long): Unit = {
            clients.get(playerId) match {
              case Some(clientActor) =>
                log(s"system: 玩家 $playerId 在线，直接发送邮件")
                clientActor ! ReceiveMails(List(mail))
              case None =>
                log(s"system: 玩家 $playerId 不在线，邮件已存入数据库")
            }
          }
          sendMessage(senderId)
          sendMessage(receiverId)
          sender() ! ReceiveReport("发送成功")
      }

    // 发送系统邮件给所有在线玩家，将邮件存入数据库和删除缓存
    case SendSystemMail(mail) =>
      log(s"system: 发送系统邮件给所有在线玩家")
      addSystemMail(mail) match {
        case Left(msg) =>
          log(s"system: 发送系统邮件 ${mail.mailId} 失败")
          log(s"system: $msg")
          sender() ! ReceiveReport(msg)
        case Right(mail) =>
          log(s"system: 发送系统邮件 ${mail.mailId} 成功")
          clients.foreach { case (_, client) =>
            client ! ReceiveMails(List(mail))
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
          log(s"system: 玩家 $playerId 领取邮件 $mailId 的附件失败")
          log(s"system: $msg")
          sender() ! ReceiveReport(msg)
        case Right(attachment) =>
          log(s"system: 玩家 $playerId 领取邮件 $mailId 的附件成功")
          sender() ! ReceiveObtainItems(attachment, mailId)
          sender() ! ReceiveCollectSuccess(mailId)
      }

    // 处理玩家删除邮件请求
    case RequestDelMail(playerId, mailId) =>
      log(s"system: 玩家 $playerId 开始删除邮件 $mailId")
      deleteMail(playerId, mailId) match {
        case Left(msg) =>
          log(s"system: 玩家 $playerId 删除邮件 $mailId 失败")
          log(s"system: $msg")
          sender() ! ReceiveReport(msg)
        case Right(_) =>
          log(s"system: 玩家 $playerId 删除邮件 $mailId 成功")
          sender() ! ReceiveDeleteSuccess(mailId)
      }

    case Terminate() =>
      log("system: 服务端关闭")
      clients.foreach { case (_, client) =>
        client ! Terminate()
      }
      //        context.stop(self)
      serverGUI.quit()
      actorSystem.terminate()

    case _ =>
      log(s"system: 服务端收到未知消息")
  }
}
