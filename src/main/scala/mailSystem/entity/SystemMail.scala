package mailSystem.entity

import java.time.LocalDateTime

case class SystemMail(
  var mailId: Long,
  var content: String,
  var title: String,
  var attachment: String,
  var filter: String,
  var publicTime: LocalDateTime,
  var deadline: LocalDateTime,
  var createTime: LocalDateTime,
  var updateTime: LocalDateTime,
  var read: Boolean,
  var collect: Boolean
) extends Mail{

  def this() = {
    this(0, "", "", "{}", "{}", null, null, null, null, false, false)
  }

  def this(title: String, content: String) = {
    this()
    this.title = title
    this.content = content
  }

  def this(title: String, content: String, attachment: String) = {
    this(title, content)
    this.attachment = attachment
  }

  def this(title: String, content: String, attachment: String, filter: String) = {
    this(title, content)
    this.attachment = attachment
    this.filter = filter
  }

  def this(title: String, content: String, attachment: String, filter: String, publicTime: LocalDateTime, deadline: LocalDateTime) = {
    this(title, content, attachment, filter)
    this.publicTime = publicTime
    this.deadline = deadline
  }

  def this (content: String, title: String, attachment: String, filter: String, publicTime: LocalDateTime, deadline: LocalDateTime, createTime: LocalDateTime, updateTime: LocalDateTime) = {
    this(title, content, attachment, filter, publicTime, deadline)
    this.createTime = createTime
    this.updateTime = updateTime
  }

  def this(mailId: Long, content: String, title: String, attachment: String, filter: String, publicTime: LocalDateTime, deadline: LocalDateTime, createTime: LocalDateTime, updateTime: LocalDateTime) = {
    this(content, title, attachment, filter, publicTime, deadline, createTime, updateTime)
    this.mailId = mailId
  }

  override def toString: String = {
    "SystemMail{" +
      "mailId=" + mailId +
      ", content='" + content + '\'' +
      ", title='" + title + '\'' +
      ", attachment='" + attachment + '\'' +
      ", filter='" + filter + '\'' +
      ", publicTime=" + publicTime +
      ", deadline=" + deadline +
      ", createTime=" + createTime +
      ", updateTime=" + updateTime +
      ", read=" + read +
      ", collect=" + collect +
      '}'
  }
}
