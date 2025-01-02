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
  var receiverId: Long
) extends Mail{

  def this() = {
    this(0, "", "", "{}", "{}", null, null, null, null, 0, 0)
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

  def this(senderId: Long, receiverId: Long, title: String, content: String, attachment: String, filter: String) = {
    this(senderId, receiverId, title)
    this.content = content
    this.attachment = attachment
    this.filter = filter
  }

  override def toString: String = {
    "PersonalMail{" +
      "senderId='" + senderId + '\'' +
      ", receiverId='" + receiverId + '\'' +
      ", mailId=" + mailId +
      ", content='" + content + '\'' +
      ", title='" + title + '\'' +
      ", attachment='" + attachment + '\'' +
      ", filter='" + filter + '\'' +
      ", publicTime=" + publicTime +
      ", deadline=" + deadline +
      ", createTime=" + createTime +
      ", updateTime=" + updateTime +
      '}'
  }
}
