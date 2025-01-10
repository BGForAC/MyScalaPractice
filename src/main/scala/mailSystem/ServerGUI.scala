package mailSystem

import akka.actor.{ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import mailSystem.MailSystemImitator.{ServerActor, Terminate, syncMailsRead}

import java.lang.Thread.sleep
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.swing._
import scala.swing.event.ButtonClicked

class ServerGUI(serverActor: ActorRef) extends SimpleSwingApplication {

  val logArea = new TextArea {
    rows = 50
    columns = 80
    editable = false
  }

  def log(message: String): Unit = {
    logArea.append(message + "\n")
  }

  def top: Frame = new MainFrame {
    title = "邮箱系统"

    val closeButton = new Button("关闭服务器")

    listenTo(closeButton)

    reactions += {
      case ButtonClicked(`closeButton`) =>
        serverActor ! Terminate()
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += closeButton
      contents += new ScrollPane(logArea)
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }
  }
}

object ServerGUIOpen {
  def main(args: Array[String]): Unit = {
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

    val scheduler = system.scheduler

    def syncRead2DBRegularly(rate: FiniteDuration) = scheduler.scheduleWithFixedDelay(0.seconds, rate)(() => syncMailsRead())

    syncRead2DBRegularly(10.seconds)
  }
}
