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
  var updateTime: LocalDateTime
) extends Mail{

  def this() = {
    this(0, "", "", "{}", "{}", null, null, null, null)
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
      '}'
  }

}
