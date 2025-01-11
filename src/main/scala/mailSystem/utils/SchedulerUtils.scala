package mailSystem.utils

import akka.actor.{ActorRef, ActorSystem, Props, Scheduler}
import mailSystem.Messages.Terminate
import mailSystem.{ClientActor, MailSystemHelper, Messages}
import mailSystem.entity.{PersonalMail, SystemMail}
import mailSystem.service.{MailService, PlayerService}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random

/**
 * 这个类是用来帮助向一个指定的serverActor发送大量的请求，以便于测试serverActor的性能
 */
case class SchedulerUtils(system: ActorSystem, serverActor: ActorRef) {
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

  def addRandomClient(): Unit = {
    // 同一个actorSystem创建同个playerId的actor会报错，此时重新创建一个playerId
    try {
      addConcreteClient(randomPlayerId)
    } catch {
      case e: Exception =>
        addRandomClient()
    }
  }

  def addConcreteClient(playerId: Long): Unit = {
    val clientActor = system.actorOf(Props(new ClientActor(playerId, serverActor, mutable.Map[Long, SystemMail](), mutable.Map[Long, PersonalMail](), mutable.Map[Long, PersonalMail](), system)), s"clientActor-$playerId")
    clients += (playerId -> clientActor)
    try {
      clientActor ! Messages.RequestAddPlayer(playerId, clientActor)
    } catch {
      case e: Exception =>
        println(s"player: 玩家 $playerId 连接失败" + e.getMessage)
        clients -= playerId
        clientActor ! Terminate()
    }
  }

  def delRandomClient(): Unit = {
    if (clients.isEmpty) return
    try {
      delConcreteClient(randomPlayerIdOnLine)
    } catch {
      case e: Exception =>
        delRandomClient()
    }
  }

  def delConcreteClient(playerId: Long): Unit = {
    clients(playerId) ! Messages.RequestDelPlayer(playerId)
    clients -= playerId
  }

  def clientConnectRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => addRandomClient())

  def readMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    val playerId = randomPlayerIdOnLine
    val mailId = randomMailIdForConcretePlayer(playerId)
    serverActor ! Messages.RequestReadMail(playerId, mailId)
  })

  def collectAttachmentRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    val playerId = randomPlayerIdOnLine
    val mailId = randomMailIdForConcretePlayer(playerId)
    serverActor ! Messages.RequestCollectAttachment(playerId, mailId)
  })

  def sendMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    @tailrec
    def loop(): Unit = {
      val senderId = randomPlayerIdOnLine
      val receiverId = randomPlayerIdOnLine
      if (senderId == receiverId) loop()
      else serverActor ! Messages.RequestSendMail(senderId, receiverId, new PersonalMail(senderId, receiverId, "title", "content", "{}", "{}"))
    }
    loop()
  })

  def sendMailToFixedReceiverRegularly(rate: FiniteDuration, fixedPlayerId: Long) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    val senderId = randomPlayerIdOnLine
    sendFixedMailRegularly(rate, senderId, fixedPlayerId)
  })

  def sendFixedMailRegularly(rate: FiniteDuration, senderId: Long, receiverId: Long) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    serverActor ! Messages.RequestSendMail(senderId, receiverId, new PersonalMail(senderId, receiverId, "title", "content", "{}", "{}"))
  })

  def sendSystemMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    serverActor ! Messages.SendSystemMail(new SystemMail("title", "content"))
  })

  def delMailRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => {
    val playerId = randomPlayerIdOnLine
    val mailId = randomMailIdForConcretePlayer(playerId)
    serverActor ! Messages.RequestDelMail(playerId, mailId)
  })
}
