package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.utils.{MapBean, SnowflakeIdGenerator}

import java.time.LocalDateTime
import scala.collection.mutable.ArrayBuffer

object MailService {
  private val snowflakeIdGeneratorForPersonalMail = new SnowflakeIdGenerator(0, 0)
  private val snowflakeIdGeneratorForSystemMail = new SnowflakeIdGenerator(0, 16)
  private val tableNameForPersonalMail = "personal_mail"
  private val tableNameForSystemMail = "system_mail"

  def systemMails(): ArrayBuffer[Mail] = {
    val sql = s"select mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time from $tableNameForSystemMail"
    val mails: ArrayBuffer[Mail] = ArrayBuffer()
    val rs = DBHelper.query(sql)
    try {
      while (rs._1.next()) {
        val mailId = rs._1.getLong("mail_id")
        val content = rs._1.getString("content")
        val title = rs._1.getString("title")
        val attachment = rs._1.getString("attachment")
        val filter = rs._1.getString("filter")
        val publicTime = rs._1.getTimestamp("public_time")
        val deadline = rs._1.getTimestamp("deadline")
        val createTime = rs._1.getTimestamp("create_time")
        val updateTime = rs._1.getTimestamp("update_time")
        if (publicTime == null || deadline == null || createTime == null || updateTime == null) {
          throw new Exception(s"Invalid mail, time is null, check the mail: $mailId in system_mail")
        }
        mails += SystemMail(mailId, content, title, attachment, filter, publicTime.toLocalDateTime, deadline.toLocalDateTime, createTime.toLocalDateTime, updateTime.toLocalDateTime)
      }
    } finally {
      DBHelper.closeRsConn(rs)
    }
    mails
  }

  def personalMails(playerId: Long): ArrayBuffer[Mail] = {
    val sql = s"select mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time, sender_id, receiver_id from $tableNameForPersonalMail where receiver_id = ?"
    val mails: ArrayBuffer[Mail] = ArrayBuffer()
    val rs = DBHelper.query(sql, playerId)
    try {
      while (rs._1.next()) {
        val mailId = rs._1.getLong("mail_id")
        val content = rs._1.getString("content")
        val title = rs._1.getString("title")
        val attachment = rs._1.getString("attachment")
        val filter = rs._1.getString("filter")
        val publicTime = rs._1.getTimestamp("public_time")
        val deadline = rs._1.getTimestamp("deadline")
        val createTime = rs._1.getTimestamp("create_time")
        val updateTime = rs._1.getTimestamp("update_time")
        val senderId = rs._1.getLong("sender_id")
        val receiverId = rs._1.getLong("receiver_id")
        if (publicTime == null || deadline == null || createTime == null || updateTime == null) {
          throw new Exception(s"Invalid mail, time is null, check the mail: $mailId in personal_mail")
        }
        mails += PersonalMail(mailId, content, title, attachment, filter, publicTime.toLocalDateTime, deadline.toLocalDateTime, createTime.toLocalDateTime, updateTime.toLocalDateTime, senderId, receiverId)
      }
    } finally {
      DBHelper.closeRsConn(rs)
    }
    mails
  }

  def mails(playerId: Long): ArrayBuffer[Mail] = {
    systemMails() ++ personalMails(playerId)
  }

  def sendMail(mail: PersonalMail): Unit = {
    require(mail.senderId > 0, "发件人不能为空")
    require(mail.receiverId > 0, "收件人不能为空")
    require(mail.title != null && mail.title.nonEmpty, "邮件标题不能为空")
    require(mail.content != null && mail.content.nonEmpty, "邮件内容不能为空")
    require(MapBean.toMutableMap(mail.filter) != MapBean.empty, "邮件过滤条件不能为空")

    val mailId = snowflakeIdGeneratorForPersonalMail.nextId()
    val sql = s"insert into $tableNameForPersonalMail (mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time, sender_id, receiver_id) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    val time = LocalDateTime.now
    DBHelper.add(sql, mailId, mail.content, mail.title, mail.attachment, mail.filter, time, time.plusMonths(1), time, time, mail.senderId, mail.receiverId)
  }

  def delMail(mailId: Long): Unit = {
    val sql = s"delete from $tableNameForPersonalMail where mail_id = ?"
    DBHelper.delete(sql, mailId)
  }

  def addSystemMail(mail: SystemMail): Unit = {
    require(mail.title != null && mail.title.nonEmpty, "邮件标题不能为空")
    require(mail.content != null && mail.content.nonEmpty, "邮件内容不能为空")
    require(MapBean.toMutableMap(mail.filter) != MapBean.empty, "邮件过滤条件不能为空")

    val mailId = snowflakeIdGeneratorForSystemMail.nextId()
    val sql = s"insert into $tableNameForSystemMail (mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time) values (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    val time = LocalDateTime.now
    DBHelper.add(sql, mailId, mail.content, mail.title, mail.attachment, mail.filter, time, time.plusMonths(1), time, time)
  }

  def delSystemMail(mailId: Long): Unit = {
    val sql = s"delete from $tableNameForSystemMail where mail_id = ?"
    DBHelper.delete(sql, mailId)
  }
}
