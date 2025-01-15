package mailSystem

import akka.actor.{ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

import scala.collection.mutable
import scala.swing._
import scala.swing.event.ButtonClicked

/**
 * 服务器GUI
 */
class ServerGUI(serverActor: ActorRef) extends SimpleSwingApplication {
  val logArea: TextArea = new TextArea {
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
        serverActor ! Messages.Terminate()
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += closeButton
      contents += new ScrollPane(logArea)
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }
  }
}

object ServerStart {
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
    if (serverActor == null) {
      println("serverActor创建失败")
    }
  }
}
