package mailSystem

import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.service.{ItemService, MailService, PlayerService}
import mailSystem.utils.{GsonUtils, MyUtils}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object MailSystem {
  def main(args: Array[String]): Unit = {
    actionCollectAttachments(5)
  }

  private def getAllPlayersId(): Array[Long] = {
    val players = PlayerService.players()
    if (players.isEmpty) throw new Exception("没有找到任何玩家")
    players.map(player => player.getPlayerId).toArray
  }

  private def getAllItemsId(): Array[Long] = {
    val items = ItemService.items()
    if (items.isEmpty) throw new Exception("没有找到任何物品")
    items.map(item => item.getItemId).toArray
  }

  private def getRandom[T](collection: Seq[T]): T = {
    collection(Random.nextInt(collection.length))
  }

  def actionLoadPlayerMails(playerId: Long): ArrayBuffer[Mail] = {
    val mails: ArrayBuffer[Mail] = MailService.mails(playerId)
    mails.foreach { mail =>
      println(mail)
    }
    mails
  }

  def actionLoadRandomPlayerMails(): (ArrayBuffer[Mail], Long) = {
    val playerId = getRandom(getAllPlayersId())
    (actionLoadPlayerMails(playerId), playerId)
  }

  private def addPlayer(name: String): Unit = {
    PlayerService.addPlayer(name)
  }

  def actionAddPlayers(count: Int): Unit = {
    for (_ <- 1 to count) {
      addPlayer(MyUtils.generateAlphaRandomLength(3)(8))
    }
  }

  private def generateRandomItemIds(offset: Int)(length: Int): Array[Long] = {
    val itemsId = getAllItemsId()
    val fixedLength = (if (offset == 0) 0 else Random.nextInt(offset)) + length
    (for (_ <- 1 to fixedLength) yield getRandom(itemsId)).toArray
  }

  private def generateRandomAttachment(): String = {
    GsonUtils.map2Json(generateRandomItemIds(0)(Random.nextInt(5)).map(id => (id.toString, MyUtils.generateNumericRandomLength(5)(0))).toMap)
  }

  private def sendMail(senderId: Long, receiverId: Long, attachment: String): Unit = {
    MailService.sendMail(new PersonalMail(senderId, receiverId, MyUtils.generateAlphaRandomLength(4)(6), MyUtils.generateAlphaRandomLength(10)(20), attachment))
  }

  def actionSendMails(count: Int): Unit = {
    val playersId = getAllPlayersId()
    val playerCount = playersId.length
    for (_ <- 1 to count) {
      val senderId = Random.nextInt(playerCount)
      val receiverId = Random.nextInt(playerCount)
      val attachment = generateRandomAttachment()
      sendMail(playersId(senderId), playersId(receiverId), attachment)
    }
  }

  private def addSystemMail(attachment: String): Unit = {
    MailService.addSystemMail(new SystemMail(MyUtils.generateAlphaRandomLength(4)(6), MyUtils.generateAlphaRandomLength(10)(20), attachment))
  }

  def actionAddSystemMails(count: Int): Unit = {
    for (_ <- 1 to count) {
      val attachment = generateRandomAttachment()
      addSystemMail(attachment)
    }
  }

  private def addItem(): Unit = {
    ItemService.addItem(MyUtils.generateLowerAlphaFixedLength(3), MyUtils.generateLowerAlphaFixedLength(10), MyUtils.generateNumericFixedLength(2).toInt)
  }

  def actionAddItems(): Unit = {
    for (_ <- 1 to 10) {
      addItem()
    }
  }

  private def readMail(mails: Seq[Mail], playerId: Long): Unit = {
    PlayerService.readMail(playerId, getRandom(mails).getMailId)
  }

  def actionReadMails(count: Int): Unit = {
    val (mails, playerId) = actionLoadRandomPlayerMails()
    println(s"玩家 $playerId 获取了邮箱")
    if (mails.isEmpty) throw new Exception("没有邮件")
    else {
      for (_ <- 1 to count) {
        readMail(mails, playerId)
      }
      println(s"玩家 $playerId 读取了邮件")
    }
  }

  private def collectAttachment(mails: Seq[Mail], playerId: Long): Unit = {
    PlayerService.collectAttachment(playerId, getRandom(mails).getMailId)
  }

  def actionCollectAttachments(count: Int): Unit = {
    val (mails, playerId) = actionLoadRandomPlayerMails()
    println(s"玩家 $playerId 获取了邮箱")
    if (mails.isEmpty) throw new Exception("没有邮件")
    else {
      for (_ <- 1 to count) {
        collectAttachment(mails, playerId)
      }
      println(s"玩家 $playerId 收取了附件")
    }
  }

}