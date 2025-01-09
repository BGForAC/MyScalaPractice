package mailSystem

import akka.actor.ActorRef

import scala.swing._
import scala.swing.event.ButtonClicked

class ClientGUI(clientActor: ActorRef) extends SimpleSwingApplication {
  val logArea = new TextArea {
    rows = 20
    columns = 50
    editable = false
  }

  def log(message: String): Unit = {
    logArea.append(message + "\n")
  }

  def top: Frame = new MainFrame {
    title = "邮箱系统"

    val connectButton = new Button {
      text = "连接到服务器"
    }

    val sendMailButton = new Button {
      text = "发送邮件"
    }

    val readMailButton = new Button {
      text = "阅读邮件"
    }

    val collectAttachmentButton = new Button {
      text = "收取附件"
    }

    val deleteMailButton = new Button {
      text = "删除邮件"
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += connectButton
      contents += sendMailButton
      contents += readMailButton
      contents += collectAttachmentButton
      contents += deleteMailButton
      contents += new ScrollPane(logArea)
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }

    listenTo(connectButton, sendMailButton, readMailButton, collectAttachmentButton, deleteMailButton)

    reactions += {
      case ButtonClicked(`connectButton`) =>
        logArea.append("正在连接到服务器...\n")
        connectToServer()

      case ButtonClicked(`sendMailButton`) =>
        logArea.append("正在发送邮件...\n")
        sendMail()

      case ButtonClicked(`readMailButton`) =>
        logArea.append("正在阅读邮件...\n")
        readMail()

      case ButtonClicked(`collectAttachmentButton`) =>
        logArea.append("正在收取附件...\n")
        collectAttachment()

      case ButtonClicked(`deleteMailButton`) =>
        logArea.append("正在删除邮件...\n")
        deleteMail()
    }

    def connectToServer(): Unit = {
      logArea.append("成功连接到数据库.\n")
    }

    def sendMail(): Unit = {
      logArea.append("邮件发送成功.\n")
    }

    def readMail(): Unit = {
      logArea.append("邮件已阅读.\n")
    }

    def collectAttachment(): Unit = {
      logArea.append("附件已收取.\n")
    }

    def deleteMail(): Unit = {
      logArea.append("邮件已删除.\n")
    }
  }
}
