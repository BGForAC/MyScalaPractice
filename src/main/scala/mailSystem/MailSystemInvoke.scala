package mailSystem

import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.service.{ItemService, MailService, PlayerService}
import mailSystem.utils.{MapBeanUtils, MyUtils}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object MailSystemInvoke {
  def main(args: Array[String]): Unit = {
    actionCollectAttachments(20)
  }

  private def allPlayersId: Array[Long] = {
    val players = PlayerService.players()

    if (players.isEmpty) Array.empty
    else players.map(player => player.getPlayerId).toArray
  }

  private def allItemsID: Array[Long] = {
    val items = ItemService.items()

    if (items.isEmpty) Array.empty
    else items.map(item => item.getItemId).toArray
  }

  private def getRandom[T](collection: Seq[T]): T = {
    collection(Random.nextInt(collection.length))
  }

  def actionLoadPlayerMails(playerId: Long): ArrayBuffer[Mail] = {
    val mails: ArrayBuffer[Mail] = MailService.mails(playerId)
    println(s"玩家 $playerId 获取了邮箱")
    mails.foreach(println)
    mails
  }

  def actionLoadRandomPlayerMails(): (ArrayBuffer[Mail], Long) = {
    val playerId = getRandom(allPlayersId)
    (actionLoadPlayerMails(playerId), playerId)
  }

  private def addPlayer(name: String): Unit = {
    PlayerService.addPlayer(name)
  }

  def actionAddPlayers(count: Int): Unit = {
    for (_ <- 1 to count) {
      addPlayer(randomPlayerName)
    }
  }

  private def generateRandomItemIds(offset: Int)(length: Int): Array[Long] = {
    val itemsId = allItemsID
    val fixedLength = (if (offset == 0) 0 else Random.nextInt(offset)) + length
    (for (_ <- 1 to fixedLength) yield getRandom(itemsId)).toArray
  }

  private def sendMail(senderId: Long, receiverId: Long, attachment: String, filter: String): Unit = {
    val newMail = new PersonalMail(senderId, receiverId, randomTitle, randomContent, attachment, filter)
    MailService.sendMail(newMail)
  }

  def actionSendMails(count: Int): Unit = {
    val playersId = allPlayersId
    val playerCount = playersId.length
    for (_ <- 1 to count) {
      val senderId = Random.nextInt(playerCount)
      val receiverId = Random.nextInt(playerCount)
      val attachment = generateRandomAttachment()
      val filter = generateRandomFilter()
      sendMail(playersId(senderId), playersId(receiverId), attachment, filter)
    }
  }

  private def addSystemMail(attachment: String, filter: String): Unit = {
    val newMail = new SystemMail(randomTitle, randomContent, attachment, filter)
    MailService.addSystemMail(newMail)
  }

  def actionAddSystemMails(count: Int): Unit = {
    for (_ <- 1 to count) {
      val attachment = generateRandomAttachment()
      val filter = generateRandomFilter()
      addSystemMail(attachment, filter)
    }
  }

  private def addItem(): Unit = {
    ItemService.addItem(randomItemName,randomItemDescription,randomItemTypeId)
  }

  def actionAddItems(): Unit = {
    for (_ <- 1 to 10) {
      addItem()
    }
  }

  private def readMail(mails: Seq[Mail], playerId: Long): Unit = {
    PlayerService.readMail(PlayerService.getPlayer(playerId), getRandom(mails).getMailId)
  }

  def actionReadMails(count: Int): Unit = {
    val (mails, playerId) = actionLoadRandomPlayerMails()
    println(s"玩家 $playerId 获取了邮箱")
    if (mails.isEmpty) throw new Exception("没有邮件")

    for (_ <- 1 to count) {
      try {
        readMail(mails, playerId)
      } catch {
        case e: Exception => println("做点什么" + e)
      }
    }
  }

  private def collectAttachment(mails: Seq[Mail], playerId: Long): Unit = {
    PlayerService.collectAttachment(PlayerService.getPlayer(playerId), getRandom(mails))
  }

  def actionCollectAttachments(count: Int): Unit = {
    val (mails, playerId) = actionLoadRandomPlayerMails()
    println(s"玩家 $playerId 获取了邮箱")
    if (mails.isEmpty) throw new Exception("没有邮件")

    for (_ <- 1 to count) {
      try {
        collectAttachment(mails, playerId)
      } catch {
        case e: Exception => println("做点什么" + e)
      }
    }
  }

  private def generateRandomAttachment(): String = {
    val map = generateRandomItemIds(0)(Random.nextInt(5)).map(id => (id.toString, Random.nextInt(1000) + 1)).toMap
    MapBeanUtils.map2Json(map)
  }

  private def generateRandomFilter(): String = {
    val map = Map("level" -> (Random.nextInt(100) + 1), "vip" -> (Random.nextInt(10) + 1))
    MapBeanUtils.map2Json(map)
  }

  private def randomTitle: String = MyUtils.generateAlphaRandomLength(4)(6)
  private def randomContent: String = MyUtils.generateAlphaRandomLength(100)(20)
  private def randomPlayerName: String = MyUtils.generateAlphaRandomLength(3)(8)
  private def randomItemDescription: String = MyUtils.generateAlphaRandomLength(20)(10)
  private def randomItemName: String = MyUtils.generateAlphaRandomLength(5)(3)
  private def randomItemTypeId: Int = MyUtils.generateNumericRandomLength(1)(2).toInt
}