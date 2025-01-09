package mailSystem

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.collection.mutable
import scala.swing._
import scala.swing.event.ButtonClicked
import MailSystemImitator.ServerActor

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

    val startButton = new Button {
      text = "启动服务器"
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += startButton
      contents += new ScrollPane(logArea)
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }

    listenTo(startButton)

    reactions += {
      case ButtonClicked(`startButton`) =>
        logArea.append("服务器启动中...\n")
        startServer()
    }

    def startServer(): Unit = {
      val system = ActorSystem("MyMailSystem")
      val serverActor = system.actorOf(Props(), "serverActor")
      logArea.append("服务器已启动\n")
    }
  }
}
