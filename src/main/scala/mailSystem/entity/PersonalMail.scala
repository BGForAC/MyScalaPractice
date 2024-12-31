package mailSystem.entity

import java.util.Date

class PersonalMail extends Mail{
  override var id: Int = _
  override var content: String = _
  override var title: String = _
  override var attachment: String = _
  override var filter: String = _
  override var deliveryTime: Date = _
  override var deadline: Date = _
  private var sender: String = _
  private var receiver: String = _

  def this(id: Int, content: String, title: String, attachment: String, filter: String, deliveryTime: Date, deadline: Date, sender: String, receiver: String) {
    this()
    this.id = id
    this.content = content
    this.title = title
    this.attachment = attachment
    this.filter = filter
    this.deliveryTime = deliveryTime
    this.deadline = deadline
    this.sender = sender
    this.receiver = receiver
  }

  def getSender: String = sender

  def setSender(sender: String): Unit = {
    this.sender = sender
  }

  def getReceiver: String = receiver

  def setReceiver(receiver: String): Unit = {
    this.receiver = receiver
  }



  override def toString: String = {
    s"PersonalMail(id=$id, content=$content, title=$title, attachment=$attachment, filter=$filter, deliveryTime=$deliveryTime, deadline=$deadline, sender=$sender, receiver=$receiver)"
  }
}
