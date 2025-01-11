package mailSystem

import akka.actor._
import com.typesafe.config.ConfigFactory
import utils.SchedulerUtils

import java.lang.Thread.sleep
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.reflect.ClassTag

/**
 * 邮件系统模拟器
 */
object MailSystemImitator {
  def createServer(): (ActorSystem, ActorRef) = {
    val config = ConfigFactory.parseString(
      """
        |akka {
        |  actor {
        |    allow-java-serialization = on
        |    serialize-messages = on
        |    provider = "akka.remote.RemoteActorRefProvider"
        |  }
        |  remote {
        |    artery {
        |      canonical.hostname = "127.0.0.1"
        |      canonical.port = 2552
        |    }
        |  }
        |}
        |""".stripMargin)
    val system = ActorSystem("MyMailSystem", config)
    val serverActor = system.actorOf(Props(new ServerActor(mutable.Map[Long, ActorRef](), system)), "serverActor")
    (system, serverActor)
  }

  def serverStart[T: ClassTag](wrapper: ((ActorSystem, ActorRef)) => Any): Unit = {
//    val classTag = implicitly[ClassTag[T]]
//    println(classTag.runtimeClass.getSimpleName)
    val config = ConfigFactory.parseString(
      """
        |akka {
        |  actor {
        |    default-dispatcher {
        |      fork-join-executor {
        |        parallelism-min = 20
        |        parallelism-factor = 3.0
        |        parallelism-max = 64
        |        task-peeking-mode = "FIFO"
        |      }
        |    }
        |    allow-java-serialization = on
        |    provider = "akka.remote.RemoteActorRefProvider"
        |  }
        |  remote {
        |    artery {
        |      canonical.hostname = "127.0.0.1"
        |      canonical.port = 0
        |    }
        |  }
        |}
        |""".stripMargin)
    val system = akka.actor.ActorSystem("MyMailSystem", config)
    val serverActorDefaultPath = s"akka://MyMailSystem@127.0.0.1:2552/user/serverActor"
    system.actorSelection(serverActorDefaultPath).resolveOne(10.seconds).onComplete({
      case scala.util.Success(serverActor) =>
        wrapper(system, serverActor)
      case scala.util.Failure(e) => println("连接失败" + e)
    })
  }

  def randomGetPlayerIdExceptSome(except: Long*): Long = {
    @scala.annotation.tailrec
    def loop(): Long = {
      val playerId: Long = MailSystemHelper.randomGetPlayerId
      if (except.contains(playerId)) loop()
      else playerId
    }

    loop()
  }

  def callWrapper[T: ClassTag](call: T => Any)(systems: (ActorSystem, ActorRef)) : Any = {
    val (system, serverActor) = systems
    val scheduler = new SchedulerUtils(system, serverActor)
    val classTag = implicitly[ClassTag[T]]
    println(classTag.runtimeClass.getSimpleName)
    call match {
      case f if classTag.runtimeClass.getSimpleName == "SchedulerUtils" => f(scheduler.asInstanceOf[T])
    }
    system.terminate().onComplete {
      case scala.util.Success(value) => println("系统关闭成功" + value)
      case scala.util.Failure(exception) => println("系统关闭失败" + exception.getMessage)
    }
  }

  private def sendRandomMail(scheduler: SchedulerUtils): Unit = {
    for (_ <- 1 to 20) {
      scheduler.addRandomClient()
    }
    sleep(10000)
    scheduler.sendMailRegularly(1.seconds)
    sleep(20000)
  }

  private def sendRandomMail2FixedPlayer(playerId: Long)(scheduler: SchedulerUtils): Unit = {
    for (_ <- 1 to 20) {
      scheduler.addRandomClient()
    }
    sleep(10000)
    scheduler.sendMailToFixedReceiverRegularly(1.seconds, playerId)
    sleep(20000)
  }

  private def sendRandomSystemMail(scheduler: SchedulerUtils): Unit = {
    for (_ <- 1 to 20) {
      scheduler.addRandomClient()
    }
    sleep(10000)
    scheduler.sendSystemMailRegularly(1.seconds)
    sleep(20000)
  }

  def main(args: Array[String]): Unit = {
//    serverStart(callWrapper(sendRandomMail))
    serverStart(callWrapper(sendRandomMail2FixedPlayer(532125159406960640L)))
//    serverStart(callWrapper(sendRandomSystemMail))
//    val server = serverStart(callWrapper) _
//    server(sendRandomMail2FixedPlayer)
//    server(sendRandomMail)
  }
}
