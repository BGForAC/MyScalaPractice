package mailSystem

import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.service.{ItemService, MailService, PlayerService}
import mailSystem.utils.{MapBeanUtils, MyUtils}

import scala.collection.mutable.ListBuffer
import scala.util.Random

object MailSystemInvoker {
  def main(args: Array[String]): Unit = {
    actionAddPlayers(100)
  }

  def allPlayersId: Array[Long] = {
    val players = PlayerService.allPlayers()

    if (players.isEmpty) Array.empty
    else players.map(player => player.getPlayerId).toArray
  }

  def allItemsID: Array[Long] = {
    val items = ItemService.items()

    if (items.isEmpty) Array.empty
    else items.map(item => item.getItemId).toArray
  }

  def getRandom[T](collection: Seq[T]): T = {
    collection(Random.nextInt(collection.length))
  }

  def randomPlayerId: Long = getRandom(allPlayersId)
  def randomItemId: Long = getRandom(allItemsID)

  def actionLoadPlayerMails(playerId: Long): ListBuffer[Mail] = {
    val mails: ListBuffer[Mail] = MailService.mails(playerId)
    println(s"玩家 $playerId 获取了邮箱")
    mails.foreach(println)
    mails
  }

  def actionLoadRandomPlayerMails(): (ListBuffer[Mail], Long) = {
    val playerId = getRandom(allPlayersId)
    (actionLoadPlayerMails(playerId), playerId)
  }

  def addPlayer(name: String): Unit = {
    PlayerService.addPlayer(name)
  }

  def actionAddPlayers(count: Int): Unit = {
    for (_ <- 1 to count) {
      addPlayer(randomPlayerName)
    }
  }

  def generateRandomItemIds(offset: Int)(length: Int): Array[Long] = {
    val itemsId = allItemsID
    val fixedLength = (if (offset == 0) 0 else Random.nextInt(offset)) + length
    (for (_ <- 1 to fixedLength) yield getRandom(itemsId)).toArray
  }

  def sendMail(senderId: Long, receiverId: Long, attachment: String, filter: String): Unit = {
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

  def addSystemMail(attachment: String, filter: String): Unit = {
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

  def addItem(): Unit = {
    ItemService.addItem(randomItemName,randomItemDescription,randomItemTypeId)
  }

  def actionAddItems(): Unit = {
    for (_ <- 1 to 10) {
      addItem()
    }
  }

  def readMail(mails: Seq[Mail], playerId: Long): Unit = {
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

  def collectAttachment(mails: Seq[Mail], playerId: Long): Unit = {
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

  def generateRandomAttachment(): String = {
    val map = generateRandomItemIds(0)(Random.nextInt(5)).map(id => (id.toString, Random.nextInt(1000) + 1)).toMap
    MapBeanUtils.map2Json(map)
  }

  def generateRandomFilter(): String = {
    val map = Map("level" -> (Random.nextInt(100) + 1), "vip" -> (Random.nextInt(10) + 1))
    MapBeanUtils.map2Json(map)
  }

  def randomTitle: String = MyUtils.generateAlphaRandomLength(4)(6)
  def randomContent: String = MyUtils.generateAlphaRandomLength(100)(20)
  def randomPlayerName: String = MyUtils.generateAlphaRandomLength(3)(8)
  def randomItemDescription: String = MyUtils.generateAlphaRandomLength(20)(10)
  def randomItemName: String = MyUtils.generateAlphaRandomLength(5)(3)
  def randomItemTypeId: Int = MyUtils.generateNumericRandomLength(1)(2).toInt
}