package mailSystem.entity

import java.time.LocalDateTime

case class PersonalMail(
  var mailId: Long,
  var content: String,
  var title: String,
  var attachment: String,
  var filter: String,
  var publicTime: LocalDateTime,
  var deadline: LocalDateTime,
  var createTime: LocalDateTime,
  var updateTime: LocalDateTime,
  var senderId: Long,
  var receiverId: Long,
  var read: Boolean,
  var collect: Boolean
) extends Mail{

  def this() = {
    this(0, "", "", "{}", "{}", null, null, null, null, 0, 0, false, false)
  }

  def this(senderId: Long, receiverId: Long, title: String) = {
    this()
    this.senderId = senderId
    this.receiverId = receiverId
    this.title = title
  }

  def this(senderId: Long, receiverId: Long, title: String, content: String) = {
    this(senderId, receiverId, title)
    this.content = content
  }

  def this(senderId: Long, receiverId: Long, title: String, content: String, attachment: String) = {
    this(senderId, receiverId, title)
    this.content = content
    this.attachment = attachment
  }

  def this(senderId: Long, receiverId: Long, title: String, content: String, attachment: String, filter: String) = {
    this(senderId, receiverId, title)
    this.content = content
    this.attachment = attachment
    this.filter = filter
  }

  def this(senderId: Long, receiverId: Long, title: String, content: String, attachment: String, filter: String, publicTime: LocalDateTime, deadline: LocalDateTime) = {
    this(senderId, receiverId, title, content, attachment, filter)
    this.publicTime = publicTime
    this.deadline = deadline
  }

  def this(senderId: Long, receiverId: Long, title: String, content: String, attachment: String, filter: String, publicTime: LocalDateTime, deadline: LocalDateTime, createTime: LocalDateTime, updateTime: LocalDateTime) = {
    this(senderId, receiverId, title, content, attachment, filter, publicTime, deadline)
    this.createTime = createTime
    this.updateTime = updateTime
  }

  def this(mailId: Long, content: String, title: String, attachment: String, filter: String, publicTime: LocalDateTime, deadline: LocalDateTime, createTime: LocalDateTime, updateTime: LocalDateTime, senderId: Long, receiverId: Long) = {
    this(senderId, receiverId, title, content, attachment, filter, publicTime, deadline, createTime, updateTime)
    this.mailId = mailId
  }

  def getSenderId: Long = senderId
  def getReceiverId: Long = receiverId

  // json格式
  override def toString: String = {
      """
      |{
      |  "mailId": %d,
      |  "content": "%s",
      |  "title": "%s",
      |  "attachment": %s,
      |  "filter": %s,
      |  "publicTime": "%s",
      |  "deadline": "%s",
      |  "createTime": "%s",
      |  "updateTime": "%s",
      |  "senderId": %d,
      |  "receiverId": %d,
      |  "read": %b,
      |  "collect": %b
      |}""".stripMargin.format(mailId, content, title, attachment, filter, publicTime, deadline, createTime, updateTime, senderId, receiverId, read, collect)

  }
}
