package mailSystem

import akka.actor.{ActorRef, Props}
import com.typesafe.config.ConfigFactory
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

  private val mailBoxStatus = new Label("")

  val logArea: TextArea = new TextArea {
    rows = 20
    columns = 50
    editable = false
  }

  private val mailTypeFilter = new ComboBox(List("所有", "系统邮件", "收件箱", "发件箱")) {
    listenTo(selection)
    reactions += {
      case _ => refreshGUI()
    }
  }

  private val mailStatusFilter = new ComboBox(List("所有", "已读", "未读", "已收取", "未收取")) {
    listenTo(selection)
    reactions += {
      case _ => refreshGUI()
    }
  }

  private def currentMailList: List[Mail] = {
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
  private var selectedMail: Option[Mail] = None

  // 展示过滤后的邮件列表，点击即可查看邮件详细信息
  private val mailList = new ListView(List.empty[Mail]) {
    selection.intervalMode = ListView.IntervalMode.Single
    listenTo(selection)
    reactions += {
      /**
       * reactions每次点击实际上触发两次，应该是点击邮件列表中的邮件时会触发一次reactions发送阅读邮件的请求，
       * 阅读邮件的从未读到已读的请求会修改ListView中当前选中项的isRead为true，再次触发reactions，最终第二次reactions时
       * selection.items.head.toString会报错，因为原先的selection.items的值被修改后已经不存在了,所以为空
       */
      case _ if selection.items.nonEmpty =>
        selectedMail = Option(selection.items.head.setReadAndReturn(true))
        mailDisplay.text = selectedMail.toString
        clientActor ! Messages.RequestReadMail(playerId, selection.items.head.getMailId)
    }
  }

  private val mailDisplay = new TextArea {
    rows = 20
    columns = 50
    editable = false
  }

  def refreshGUI(): Unit = {
    println("刷新GUI")
    mailBoxStatus.text = s"邮件箱状态: 收件箱(${receiveMails.size}/100) 发件箱(${sendMails.size}) 系统邮件(${systemMails.size})"
    mailList.listData = currentMailList
  }

  def refreshCollectAttachment(mailId: Long): Unit = {
    println("刷新附件")
    selectedMail match {
      case Some(mail) if mail.getMailId == mailId =>
        mail.setCollect(true)
        mailDisplay.text = mail.toString
      case _ =>
    }
  }

  def log(message: String): Unit = {
    logArea.append(message + "\n")
  }

  def top: Frame = new MainFrame {
    title = s"邮箱系统,用户 $playerId"

    val sendMailButton: Button = new Button {
      text = "发送邮件"
    }

    val collectAttachmentButton: Button = new Button {
      text = "收取附件"
    }

    val deleteMailButton: Button = new Button {
      text = "删除邮件"
    }

    val disConnectButton: Button = new Button {
      text = "断开连接"
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += mailBoxStatus
      }
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
      selectedMail match {
        case Some(mail) if !mail.haveAttachment =>
          Dialog.showMessage(null, "没有附件", "错误", Dialog.Message.Error)
        case Some(mail) if mail.haveAttachment && !mail.isCollect =>
          clientActor ! Messages.RequestCollectAttachment(playerId, mail.getMailId)
        case Some(mail) if mail.isCollect =>
          Dialog.showMessage(null, "附件已收取", "错误", Dialog.Message.Error)
        case _ =>
          Dialog.showMessage(null, "请选择要收取附件的邮件", "错误", Dialog.Message.Error)
      }
    }

    def deleteMail(): Unit = {
      selectedMail match {
        case Some(mail) =>
          clientActor ! Messages.RequestDelMail(playerId, mail.getMailId)
        case _ =>
          Dialog.showMessage(null, "请选择要删除的邮件", "错误", Dialog.Message.Error)
      }
    }

    def disconnect(): Unit = {
      clientActor ! Messages.RequestDelPlayer(playerId)
    }
  }

  private case class SendMailDialog() extends Dialog {
    title = "发送邮件"
    modal = true

    private val receiverIdField = new TextField(10)
    private val titleField = new TextField(20)
    private val contentField = new TextArea {
      rows = 10
      columns = 50
    }

    private val sendButton = new Button {
      text = "发送"
    }

    private val cancelButton: Button = new Button {
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
        sendMail()
        close()

      case ButtonClicked(`cancelButton`) =>
        close()
    }

    private def sendMail(): Unit = {
      require(receiverIdField.text.nonEmpty, "收件人ID不能为空")
      require(titleField.text.nonEmpty, "标题不能为空")
      require(contentField.text.nonEmpty, "内容不能为空")

      val receiverId = receiverIdField.text.toLong
      clientActor ! Messages.RequestSendMail(playerId, receiverId, new PersonalMail(playerId, receiverId, titleField.text, contentField.text))
    }
  }
}

object ClientLogin extends SimpleSwingApplication {

  def top: Frame = new MainFrame{
    title = "登录"
    val serverAddress = new TextField(30)
    val playerIdField = new TextField(30)
    val loginButton: Button = new Button {
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
      val serverActorPath = s"akka://MyMailSystem@$serverAddressStr/user/serverActor"
      val serverActorDefaultPath = s"akka://MyMailSystem@127.0.0.1:2552/user/serverActor"
      system.actorSelection(serverActorDefaultPath).resolveOne(10.seconds).onComplete{
        case scala.util.Success(serverActor) =>
          val clientActor = system.actorOf(Props(new ClientActor(playerId, serverActor, systemMails, receiveMails, sendMails, system)), "clientActor")
          clientActor ! Messages.RequestAddPlayer(playerId, clientActor)
          this.close()
        case scala.util.Failure(e) => println("连接失败" + e)
      }
    }
  }
}