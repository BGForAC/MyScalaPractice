package mailSystem

import akka.actor.{Actor, ActorRef, ActorSystem}
import Messages._
import mailSystem.entity.{Item, Mail, PersonalMail, SystemMail}

import scala.collection.mutable

/**
 * 客户端Actor
 * 当使用同一个actorSystem创建多个客户端时，需要关闭部分客户端时，需要修改RequestDelPlayer的实现，不再关闭actorSystem，而是关闭Actor，Terminate也一起改
 */
class ClientActor(val playerId: Long,
                  val server: ActorRef,
                  val systemMails: mutable.Map[Long, SystemMail],
                  val personalMails: mutable.Map[Long, PersonalMail],
                  val sendMails: mutable.Map[Long, PersonalMail],
                  val actorSystem: ActorSystem) extends Actor {

  val items: mutable.Map[Item, Int] = mutable.Map()

  private val clientGUI = new ClientGUI(self, playerId, systemMails, personalMails, sendMails, items)
  clientGUI.main(Array())

  override def postStop(): Unit = {
    server ! RequestDelPlayer(playerId)
    clientGUI.quit()
  }

  private def refreshGUI(): Unit = {
    clientGUI.refreshGUI()
  }

  private def refreshItem(): Unit = {
    clientGUI.refreshItem()
  }

  private def refreshGUIAttachmentCollect(mailId: Long): Unit = {
    clientGUI.refreshCollectAttachment(mailId)
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

  private def collectItem(item: Item, quantity: Int): Unit = {
    log(s"player: 玩家 $playerId 收到物品 ${item.getName} * $quantity")
    if (items.contains(item)) items(item) += quantity
    else items += (item -> quantity)
  }

  override def receive: Receive = {
    // 为GUI提供日志
    case Log(content) =>
      log(content)

    case Terminate() =>
      log(s"player: 服务端关闭，玩家 $playerId 断开连接")
      //        context.stop(self)
      actorSystem.terminate()

    // 向服务端发送连接请求
    case RequestAddPlayer(playerId, client) =>
      log(s"player: 玩家 $playerId 请求连接")
      server ! RequestAddPlayer(playerId, client)

    // 从服务端断开连接
    case RequestDelPlayer(playerId) =>
      log(s"player: 玩家 $playerId 请求断开连接")
      //        context.stop(self)
      actorSystem.terminate()

    // 一般发生在玩家登陆时，加载邮件到本地
    case RequestLoadMails(playerId) =>
      log(s"player: 玩家 $playerId 请求拉取邮箱到本地")
      server ! RequestLoadMails(playerId)

    // 一般发生在玩家登陆时，加载物品到本地
    case RequestLoadItems(playerId) =>
      log(s"player: 玩家 $playerId 请求拉取物品到本地")
      server ! RequestLoadItems(playerId)

    // 发送邮件
    case RequestSendMail(sender, receiver, mail) =>
      log(s"player: 玩家 $sender 请求发送邮件给玩家 $receiver")
      server ! RequestSendMail(playerId, receiver, mail)

    // 玩家收到邮件时，将邮件存入本地
    case ReceiveMails(mails) =>
      mails.foreach(addMail)
      refreshGUI()

    // 玩家收到物品时，打印物品信息
    case ReceiveItems(items) =>
      items.foreach{ case (item, quantity) => collectItem(item, quantity) }
      refreshItem()

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
    case ReceiveObtainItems(items, mailId) =>
      log(s"player: 玩家 $playerId 领取邮件 $mailId 的附件成功")
      items.foreach { case (item, quantity) => collectItem(item, quantity) }
      refreshGUIAttachmentCollect(mailId)
      refreshItem()

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
