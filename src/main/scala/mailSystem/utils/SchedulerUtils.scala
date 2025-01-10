package mailSystem.utils

import akka.actor.{ActorRef, ActorSystem, Props, Scheduler}
import mailSystem.{ClientActor, MailSystemHelper, Messages}
import mailSystem.entity.{PersonalMail, SystemMail}
import mailSystem.service.{MailService, PlayerService}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random

/**
 * 这个类是用来帮助向一个指定的serverActor发送大量的请求，以便于测试serverActor的性能
 * @param system
 * @param serverActor
 */
class SchedulerUtils(system: ActorSystem, serverActor: ActorRef) {
  private val clients = mutable.Map[Long, ActorRef]()

  private val scheduler = system.scheduler

  private def randomPlayerId = MailSystemHelper.randomGetPlayerId

  private def randomMailIdForConcretePlayer(playerId: Long) = {
    val mails = MailService.mails(playerId)
    MyUtils.getRandomElementInCollection(mails).getMailId
  }

  private val fixedPlayer = PlayerService.getPlayer(randomPlayerId)

  private def randomMailIdForFixedPlayerId = randomMailIdForConcretePlayer(fixedPlayer.getPlayerId)

  private val fixedMailId = randomMailIdForFixedPlayerId

  private def randomPlayerIdOnLine = clients.keySet.toSeq(Random.nextInt(clients.size))

  private def randomClientActor = clients(randomPlayerIdOnLine)

  def addRandomClient(): Unit = addConcreteClient(randomPlayerId)

  def addConcreteClient(playerId: Long): Unit = {
    val clientActor = system.actorOf(Props(new ClientActor(playerId, serverActor, mutable.Map[Long, SystemMail](), mutable.Map[Long, PersonalMail](), mutable.Map[Long, PersonalMail](), system)), s"clientActor-$playerId")
    clientActor ! Messages.RequestAddPlayer(playerId, clientActor)
  }

  def delRandomClient(): Unit = {
    val playerId = randomPlayerIdOnLine
    delConcreteClient(playerId)
  }

  def delConcreteClient(playerId: Long): Unit = {
    clients(playerId) ! Messages.RequestDelPlayer(playerId)
  }

  def clientConnectRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => addRandomClient())

  def readMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    val playerId = randomPlayerIdOnLine
    val mailId = randomMailIdForConcretePlayer(playerId)
    val clientActor = clients(playerId)
    clientActor ! Messages.RequestReadMail(playerId, mailId)
  })

  def collectAttachmentRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    val playerId = randomPlayerIdOnLine
    val mailId = randomMailIdForConcretePlayer(playerId)
    val clientActor = clients(playerId)
    clientActor ! Messages.RequestCollectAttachment(playerId, mailId)
  })

  def sendMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    val sender = randomPlayerIdOnLine
    val receiver =  randomPlayerIdOnLine
    sendFixedMailRegularly(rate, sender, receiver)
  })

  def sendFixedMailRegularly(rate: FiniteDuration, senderId: Long, receiverId: Long) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    val clientActor = clients(senderId)
    clientActor ! Messages.RequestSendMail(senderId, receiverId, new PersonalMail(senderId, receiverId, "title", "content", "{}", "{}"))
  })

  def sendSystemMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    serverActor ! Messages.SendSystemMail(new SystemMail("title", "content"))
  })

  def delMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    val playerId = randomPlayerIdOnLine
    val mailId = randomMailIdForConcretePlayer(playerId)
    val clientActor = clients(playerId)
    clientActor ! Messages.RequestDelMail(playerId, mailId)
  })
}
