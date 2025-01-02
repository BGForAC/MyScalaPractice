package mailSystem

import mailSystem.entity.Mail
import mailSystem.service.{ItemService, MailService, PlayerService}
import mailSystem.utils.MyUtils
import mailSystem.utils.MyUtils.RandomGenerator

import scala.collection.mutable.ListBuffer

object MailSystem {
  private val generateRandomLengthAlpha = MyUtils.generateRandomString(RandomGenerator.generateRandomAlpha) _
  private val generateRandomLengthLowerAlpha = MyUtils.generateRandomString(RandomGenerator.generateRandomLowerAlpha) _
  private val generateRandomLengthUpperAlpha = MyUtils.generateRandomString(RandomGenerator.generateRandomUpperAlpha) _
  private val generateRandomLengthNumeric = MyUtils.generateRandomString(RandomGenerator.generateRandomNumeric) _

  def main(args: Array[String]): Unit = {
  }

  def actionLoadPlayerMails(playerId: Long): Unit = {
    val mails: ListBuffer[Mail] = MailService.mails(playerId)
    mails.foreach { mail =>
      println(mail)
    }
  }

  def actionAddPlayer(name: String): Unit = {
    PlayerService.addUser(name)
  }

  def actionAddPlayers(): Unit = {
    for (_ <- 1 to 10) {
      PlayerService.addUser(generateRandomLengthAlpha(3)(8))
    }
  }

  def actionSendMail(senderId: Long, receiverId: Long, title: String, content: String, attachment: String): Unit = {
    MailService.sendMail(senderId, receiverId, title, content, attachment)
  }

  def actionSendMails(): Unit = {
    for (_ <- 1 to 10) {
      actionSendMail(1, 2, MyUtils.RandomGenerator.generateRandomAlpha(10), MyUtils.RandomGenerator.generateRandomAlpha(10), MyUtils.RandomGenerator.generateRandomAlpha(10))
    }
  }

  def actionAddSystemMail(): Unit = {
    MailService.addSystemMail(1, 2, MyUtils.RandomGenerator.generateRandomAlpha(10), MyUtils.RandomGenerator.generateRandomAlpha(10), MyUtils.RandomGenerator.generateRandomAlpha(10))
  }

  def actionAddSystemMails(): Unit = {
    for (_ <- 1 to 10) {
      actionAddSystemMail()
    }
  }

  def actionAddItem(): Unit = {
    ItemService.addItem(RandomGenerator.generateRandomLowerAlpha(4), RandomGenerator.generateRandomLowerAlpha(10), RandomGenerator.generateRandomNumeric(2).toInt)
  }

  def actionAddItems(): Unit = {
    for (_ <- 1 to 10) {
      actionAddItem()
    }
  }
}