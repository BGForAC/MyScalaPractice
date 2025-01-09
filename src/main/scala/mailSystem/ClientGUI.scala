package mailSystem

import akka.actor.ActorRef
import mailSystem.entity.{Mail, PersonalMail, SystemMail}

import scala.collection.mutable
import scala.swing._
import scala.swing.event.ButtonClicked

class ClientGUI(clientActor: ActorRef,
                systemMails: mutable.Map[Long, SystemMail],
                receiveMails: mutable.Map[Long, PersonalMail],
                sendMails: mutable.Map[Long, PersonalMail]) extends SimpleSwingApplication {
  val logArea = new TextArea {
    rows = 20
    columns = 50
    editable = false
  }

  val mailTypeFilter = new ComboBox(List("所有", "系统邮件", "收件箱", "发件箱")){
    listenTo(selection)
    reactions += {
      case _ => refreshGUI()
    }
  }

  val mailStatusFilter = new ComboBox(List("所有", "已读", "未读", "已收取", "未收取")){
    listenTo(selection)
    reactions += {
      case _ => refreshGUI()
    }
  }

  var currentMailList: List[Mail] = List.empty

  def updateCurrentMailList: Unit = {
    val mailList = mailTypeFilter.selection.item match {
      case "系统邮件" => systemMails.values
      case "收件箱" => receiveMails.values
      case "发件箱" => sendMails.values
      case _ => systemMails.values ++ receiveMails.values ++ sendMails.values
    }
    currentMailList = mailList.filter(mail => mailStatusFilter.selection.item match {
      case "已读" => mail.isRead
      case "未读" => !mail.isRead
      case "已收取" => mail.isCollect
      case "未收取" => !mail.isCollect && mail.haveAttachment
      case _ => true
    }).toList
  }

  // 展示过滤后的邮件列表，点击即可查看邮件详细信息
  val mailList = new ListView(List.empty[String]) {
    selection.intervalMode = ListView.IntervalMode.Single
    listenTo(selection)
    reactions += {
      case _ => mailDisplay.text = currentMailList(selection.leadIndex).toString
    }
  }

  val mailDisplay = new TextArea {
    rows = 20
    columns = 50
    editable = false
  }

//  val systemMails = mutable.Map[Long, SystemMail]()
//  val receiveMails = mutable.Map[Long, PersonalMail]()
//  val sendMails = mutable.Map[Long, PersonalMail]()

//  def refreshSystemMailList(systemMails: mutable.Map[Long, SystemMail]) = {
//    this.systemMails.clear()
//    this.systemMails ++= systemMails
//  }
//
//  def refreshReceiveMailList(receiveMails: mutable.Map[Long, PersonalMail]) = {
//    this.receiveMails.clear()
//    this.receiveMails ++= receiveMails
//  }
//
//  def refreshSendMailList(sendMails: mutable.Map[Long, PersonalMail]) = {
//    this.sendMails.clear()
//    this.sendMails ++= sendMails
//  }
//
  def refreshGUI(): Unit = {
    updateCurrentMailList
    mailList.listData = currentMailList.map(_.getTitle)
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
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += connectButton
        contents += sendMailButton
        contents += readMailButton
        contents += collectAttachmentButton
        contents += deleteMailButton
      }
      contents += new ScrollPane(logArea)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += mailTypeFilter
        contents += mailStatusFilter
      }
      contents += new ScrollPane(mailList)
      contents += new ScrollPane(mailDisplay)
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

object ClientGUITest extends App {
}