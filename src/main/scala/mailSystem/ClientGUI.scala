package mailSystem

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.config.ConfigFactory
import mailSystem.MailSystemImitator.{ClientActor, RequestGetClientActor}
import mailSystem.entity.{Mail, PersonalMail, SystemMail}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.swing._
import scala.swing.event.ButtonClicked

class ClientGUI(clientActor: ActorRef,
                playerId: Long,
                systemMails: mutable.Map[Long, SystemMail],
                receiveMails: mutable.Map[Long, PersonalMail],
                sendMails: mutable.Map[Long, PersonalMail]) extends SimpleSwingApplication {
  val logArea = new TextArea {
    rows = 20
    columns = 50
    editable = false
  }

  val mailTypeFilter = new ComboBox(List("所有", "系统邮件", "收件箱", "发件箱")) {
    listenTo(selection)
    reactions += {
      case _ => refreshGUI()
    }
  }

  val mailStatusFilter = new ComboBox(List("所有", "已读", "未读", "已收取", "未收取")) {
    listenTo(selection)
    reactions += {
      case _ => refreshGUI()
    }
  }

  def currentMailList: List[Mail] = {
    val mailList = mailTypeFilter.selection.item match {
      case "系统邮件" => systemMails.values
      case "收件箱" => receiveMails.values
      case "发件箱" => sendMails.values
      case _ => systemMails.values ++ receiveMails.values ++ sendMails.values
    }
    mailList.filter(mail => mailStatusFilter.selection.item match {
      case "已读" => mail.isRead
      case "未读" => !mail.isRead
      case "已收取" => mail.isCollect
      case "未收取" => !mail.isCollect && mail.haveAttachment
      case _ => true
    }).toList
  }

  // 选中的邮件，因为每次更新都会重新赋值ListView的listData，所以需要一个变量来保存选中的邮件
  var selectedMail: Mail = null

  // 展示过滤后的邮件列表，点击即可查看邮件详细信息
  val mailList = new ListView(List.empty[Mail]) {
    selection.intervalMode = ListView.IntervalMode.Single
    listenTo(selection)
    reactions += {
      /**
       * reactions每次点击实际上触发两次，应该是点击邮件列表中的邮件时会触发一次reactions发送阅读邮件的请求，
       * 阅读邮件的从未读到已读的请求会修改ListView中当前选中项的isRead为true，再次触发reactions，最终第二次reactions时
       * selection.items.head.toString会报错，因为原先的selection.items的值被修改后已经不存在了,所以为空
       */
      case _ if selection.items.nonEmpty =>
        selectedMail = selection.items.head.asInstanceOf[Mail].setReadAndReturn(true)
        mailDisplay.text = selectedMail.toString
        clientActor ! MailSystemImitator.RequestReadMail(playerId, selection.items.head.asInstanceOf[Mail].getMailId)
    }
  }

  val mailDisplay = new TextArea {
    rows = 20
    columns = 50
    editable = false
  }

  def refreshGUI(): Unit = {
    println("刷新GUI")
    mailList.listData = currentMailList
  }

  def refreshCollectAttachment(mailId: Long): Unit = {
    println("刷新附件")
    if (selectedMail != null && selectedMail.getMailId == mailId) {
      selectedMail.setCollect(true)
      mailDisplay.text = selectedMail.toString
    }
  }

  def log(message: String): Unit = {
    logArea.append(message + "\n")
  }

  def top: Frame = new MainFrame {
    title = s"邮箱系统,用户 $playerId"

    val sendMailButton = new Button {
      text = "发送邮件"
    }

    val collectAttachmentButton = new Button {
      text = "收取附件"
    }

    val deleteMailButton = new Button {
      text = "删除邮件"
    }

    val disConnectButton = new Button {
      text = "断开连接"
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += sendMailButton
        contents += collectAttachmentButton
        contents += deleteMailButton
        contents += disConnectButton
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

    listenTo(sendMailButton, collectAttachmentButton, deleteMailButton, disConnectButton)

    reactions += {
      case ButtonClicked(`sendMailButton`) =>
        logArea.append("正在发送邮件...\n")
        sendMail()

      case ButtonClicked(`collectAttachmentButton`) =>
        logArea.append("正在收取附件...\n")
        collectAttachment()

      case ButtonClicked(`deleteMailButton`) =>
        logArea.append("正在删除邮件...\n")
        deleteMail()

      case ButtonClicked(`disConnectButton`) =>
        logArea.append("正在断开连接...\n")
        disconnect()
    }

    def sendMail(): Unit = {
      SendMailDialog().open()
    }

    def collectAttachment(): Unit = {
      clientActor ! MailSystemImitator.RequestCollectAttachment(playerId, selectedMail.getMailId)
    }

    def deleteMail(): Unit = {
      clientActor ! MailSystemImitator.RequestDelMail(playerId, selectedMail.getMailId)
    }

    def disconnect(): Unit = {
      clientActor ! MailSystemImitator.RequestDelPlayer(playerId)
    }
  }

  case class SendMailDialog() extends Dialog {
    title = "发送邮件"
    modal = true

    val receiverIdField = new TextField(10)
    val titleField = new TextField(20)
    val contentField = new TextArea {
      rows = 10
      columns = 50
    }

    val sendButton = new Button {
      text = "发送"
    }

    val cancelButton = new Button {
      text = "取消"
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("收件人ID:")
        contents += receiverIdField
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("标题:")
        contents += titleField
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("内容:")
        contents += contentField
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += sendButton
        contents += cancelButton
      }
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }

    listenTo(sendButton, cancelButton)

    reactions += {
      case ButtonClicked(`sendButton`) =>
        require(receiverIdField.text.nonEmpty, "收件人ID不能为空")
        require(titleField.text.nonEmpty, "标题不能为空")
        require(contentField.text.nonEmpty, "内容不能为空")

        val receiverId = receiverIdField.text.toLong
        clientActor ! MailSystemImitator.RequestSendMail(playerId, receiverId, new PersonalMail(playerId, receiverId, titleField.text, contentField.text))
        close()

      case ButtonClicked(`cancelButton`) =>
        close()
    }
  }
}

object ClientLogin extends SimpleSwingApplication {

  def top: Frame = new MainFrame{
    title = "登录"
    val serverAddress = new TextField(30)
    val playerIdField = new TextField(30)
    val loginButton = new Button {
      text = "登录"
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("服务器地址:")
        contents += serverAddress
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("玩家ID:")
        contents += playerIdField
      }
      contents += loginButton
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }

    listenTo(loginButton)

    reactions += {
      case ButtonClicked(`loginButton`) =>
        try {
          connect()
        } catch {
          case e: Exception =>
            Dialog.showMessage(null, e.getMessage, "错误", Dialog.Message.Error)
        }
    }

    def connect(): Unit = {
      val config = ConfigFactory.parseString(
        """
          |akka {
          |  actor {
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
      val serverAddressStr = serverAddress.text
      val playerId = playerIdField.text.toLong
      val systemMails = mutable.Map[Long, SystemMail]()
      val receiveMails = mutable.Map[Long, PersonalMail]()
      val sendMails = mutable.Map[Long, PersonalMail]()
      val system = akka.actor.ActorSystem("MyMailSystem", config)
//      val serverActorPath = s"akka://MyMailSystem@$serverAddressStr/user/serverActor"
      val serverActorPath = s"akka://MyMailSystem@127.0.0.1:2552/user/serverActor"
      val serverActor = system.actorSelection(serverActorPath).resolveOne(10.seconds).onComplete{
        case scala.util.Success(serverActor) =>
          val clientActor = system.actorOf(Props(new ClientActor(playerId, serverActor, systemMails, receiveMails, sendMails, system)), "clientActor")
          clientActor ! MailSystemImitator.RequestAddPlayer(playerId, clientActor)
          this.close()
        case scala.util.Failure(exception) => println("连接失败")
      }
    }
  }
}