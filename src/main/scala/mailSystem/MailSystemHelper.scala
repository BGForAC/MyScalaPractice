package mailSystem

import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.service.{ItemService, MailService, PlayerService}
import mailSystem.utils.{MapBeanUtils, MyUtils}

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * 这个类是用来大量生成和获取数据库中的各种数据的
 */
object MailSystemHelper {
  def main(args: Array[String]): Unit = {
    addPlayers(100)
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

  def loadConcrePlayersMails(playerId: Long): ListBuffer[Mail] = {
    val mails: ListBuffer[Mail] = MailService.mails(playerId)
    mails.foreach(println)
    mails
  }

  def loadRandomPlayerMails(): (ListBuffer[Mail], Long) = {
    val playerId = MyUtils.getRandomElementInCollection(allPlayersId)
    (loadConcrePlayersMails(playerId), playerId)
  }

  def addPlayer(name: String): Unit = {
    PlayerService.addPlayer(name)
  }

  def addPlayers(count: Int): Unit = {
    for (_ <- 1 to count) {
      addPlayer(randomPlayerName)
    }
  }

  def sendMail(senderId: Long, receiverId: Long, attachment: String, filter: String): Unit = {
    val newMail = new PersonalMail(senderId, receiverId, randomTitle, randomContent, attachment, filter)
    MailService.addPersonalMail(senderId, receiverId, newMail)
  }

  def sendMails(count: Int): Unit = {
    val playersId = allPlayersId
    val playerCount = playersId.length
    for (_ <- 1 to count) {
      val senderId = Random.nextInt(playerCount)
      val receiverId = Random.nextInt(playerCount)
      val attachment = randomAttachment
      val filter = randomFilter
      if (senderId != receiverId) sendMail(playersId(senderId), playersId(receiverId), attachment, filter)
    }
  }

  def addSystemMail(): Unit = {
    val newMail = new SystemMail(randomTitle, randomContent, randomAttachment, randomFilter)
    MailService.addSystemMail(newMail)
  }

  def addSystemMails(count: Int): Unit = {
    for (_ <- 1 to count) {
      addSystemMail()
    }
  }

  def addItem(): Unit = {
    ItemService.addItem(randomItemName,randomItemDescription,randomItemTypeId)
  }

  def addItems(): Unit = {
    for (_ <- 1 to 10) {
      addItem()
    }
  }

  def readMail(mails: Seq[Mail], playerId: Long): Unit = {
    PlayerService.readMail(PlayerService.getPlayer(playerId), MyUtils.getRandomElementInCollection(mails).getMailId)
  }

  def readMails(count: Int): Unit = {
    val (mails, playerId) = loadRandomPlayerMails()
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
    PlayerService.collectAttachment(PlayerService.getPlayer(playerId), MyUtils.getRandomElementInCollection(mails))
  }

  def collectAttachments(count: Int): Unit = {
    val (mails, playerId) = loadRandomPlayerMails()
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

  // randomGet指从已有的数据中随机获取，random指生成随机数据

  def randomGetPlayerId: Long = MyUtils.getRandomElementInCollection(allPlayersId)
  def randomGetItemId: Long = MyUtils.getRandomElementInCollection(allItemsID)

  def randomAttachment: String = {
    val map = randomItemId(0)(Random.nextInt(5)).map(id => (id.toString, Random.nextInt(1000) + 1)).toMap
    MapBeanUtils.map2Json(map)
  }

  def randomFilter: String = {
    val map = Map("level" -> (Random.nextInt(100) + 1), "vip" -> (Random.nextInt(10) + 1))
    MapBeanUtils.map2Json(map)
  }
  def randomItemId(offset: Int)(length: Int): Array[Long] = {
    val itemsId = allItemsID
    val fixedLength = (if (offset == 0) 0 else Random.nextInt(offset)) + length
    (for (_ <- 1 to fixedLength) yield MyUtils.getRandomElementInCollection(itemsId)).toArray
  }
  def randomTitle: String = MyUtils.generateAlphaRandomLength(4)(6)
  def randomContent: String = MyUtils.generateAlphaRandomLength(100)(20)
  def randomPlayerName: String = MyUtils.generateAlphaRandomLength(3)(8)
  def randomItemDescription: String = MyUtils.generateAlphaRandomLength(20)(10)
  def randomItemName: String = MyUtils.generateAlphaRandomLength(5)(3)
  def randomItemTypeId: Int = MyUtils.generateNumericRandomLength(1)(2).toInt
}