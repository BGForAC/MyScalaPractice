package mailSystem

import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.service.{ItemService, MailService, PlayerService}
import mailSystem.utils.MyUtils

import scala.collection.mutable.ListBuffer
import scala.util.Random

object MailSystem {
  def main(args: Array[String]): Unit = {
  }

  def getAllPlayersId(): Array[Long] = {
    val players = PlayerService.players()
    if (players.isEmpty) throw new Exception("No player found")
    players.map(player => player.getPlayerId).toArray
  }

  def getAllItemsId(): Array[Long] = {
    val items = ItemService.items()
    if (items.isEmpty) throw new Exception("No item found")
    items.map(item => item.getItemId).toArray
  }

  def actionLoadPlayerMails(playerId: Long): Unit = {
    val mails: ListBuffer[Mail] = MailService.mails(playerId)
    mails.foreach { mail =>
      println(mail)
    }
  }

  def actionLoadRandomPlayerMails(): Unit = {
    val playersId = getAllPlayersId()
    val playerCount = playersId.length
    val playerId = playersId(Random.nextInt(playerCount))
    actionLoadPlayerMails(playerId)
  }

  private def actionAddPlayer(name: String): Unit = {
    PlayerService.addPlayer(name)
  }

  def actionAddPlayers(count: Int): Unit = {
    for (_ <- 1 to count) {
      PlayerService.addPlayer(MyUtils.generateAlphaRandomLength(3)(8))
    }
  }

  private def actionSendMail(senderId: Long, receiverId: Long): Unit = {
    MailService.sendMail(new PersonalMail(senderId, receiverId, MyUtils.generateAlphaRandomLength(4)(6), MyUtils.generateAlphaRandomLength(10)(20)))
  }

  def actionSendMails(count: Int): Unit = {
    val playersId = getAllPlayersId()
    val playerCount = playersId.length
    for (_ <- 1 to count) {
      val senderId = Random.nextInt(playerCount)
      val receiverId = Random.nextInt(playerCount)
      actionSendMail(playersId(senderId), playersId(receiverId))
    }
  }

  private def actionAddSystemMail(): Unit = {
    MailService.addSystemMail(new SystemMail())
  }

  def actionAddSystemMails(): Unit = {
    for (_ <- 1 to 10) {
      actionAddSystemMail()
    }
  }

  private def actionAddItem(): Unit = {
    ItemService.addItem(MyUtils.generateLowerAlphaFixedLength(3), MyUtils.generateLowerAlphaFixedLength(10), MyUtils.generateNumericFixedLength(2).toInt)
  }

  def actionAddItems(): Unit = {
    for (_ <- 1 to 10) {
      actionAddItem()
    }
  }
}