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

  def getMailId: Long = mailId
  def getContent: String = content
  def getTitle: String = title
  def getAttachment: String = attachment
  def getFilter: String = filter
  def getPublicTime: LocalDateTime = publicTime
  def getDeadline: LocalDateTime = deadline
  def getCreateTime: LocalDateTime = createTime
  def getUpdateTime: LocalDateTime = updateTime
}