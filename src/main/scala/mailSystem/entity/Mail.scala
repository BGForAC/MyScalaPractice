package mailSystem.entity

import java.time.LocalDateTime

trait Mail {
  protected var mailId: Long
  protected var content: String
  protected var title: String
  protected var attachment: String
  protected var filter: String
  protected var publicTime: LocalDateTime
  protected var deadline: LocalDateTime
  protected var createTime: LocalDateTime
  protected var updateTime: LocalDateTime
  protected var read: Boolean
  protected var collect: Boolean

  def getMailId: Long = mailId
  def getContent: String = content
  def getTitle: String = title
  def getAttachment: String = attachment
  def getFilter: String = filter
  def getPublicTime: LocalDateTime = publicTime
  def getDeadline: LocalDateTime = deadline
  def getCreateTime: LocalDateTime = createTime
  def getUpdateTime: LocalDateTime = updateTime
  def isRead: Boolean = read
  def isCollect: Boolean = collect
  def haveAttachment: Boolean = attachment != "{}"

  def setRead(read: Boolean): Unit = this.read = read
  def setReadAndReturn(read: Boolean): Mail = {
    this.read = read
    this
  }
  def setCollect(collect: Boolean): Unit = this.collect = collect

  def status: String = if (read && !collect) "未领取" else if (read && collect) "已领取" else "未读"

  override def equals(obj: Any): Boolean = obj match {
    case mail: Mail => mail.getMailId == this.getMailId
    case _ => false
  }

  override def hashCode(): Int = mailId.hashCode()
}