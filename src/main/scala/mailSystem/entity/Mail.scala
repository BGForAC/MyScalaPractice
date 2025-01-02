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
}
