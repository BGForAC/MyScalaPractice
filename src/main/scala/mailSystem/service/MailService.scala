package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.utils.SnowflakeIdGenerator

import java.time.LocalDateTime
import scala.collection.mutable.ListBuffer

object MailService {
  private val snowflakeIdGenerator = new SnowflakeIdGenerator(0, 0)

  def systemMails(): ListBuffer[Mail] = {
    val sql = "select * from system_mail"
    val rs = DBHelper.query(sql)
    val mails: ListBuffer[Mail] = ListBuffer()
    while (rs.next()) {
      val mailId = rs.getLong("mail_id")
      val content = rs.getString("content")
      val title = rs.getString("title")
      val attachment = rs.getString("attachment")
      val filter = rs.getString("filter")
      val publicTime = rs.getTimestamp("public_time")
      val deadline = rs.getTimestamp("deadline")
      val createTime = rs.getTimestamp("create_time")
      val updateTime = rs.getTimestamp("update_time")
      if (publicTime == null || deadline == null || createTime == null || updateTime == null) {
        throw new Exception(s"Invalid mail, time is null, check the mail: $mailId in system_mail")
      }
      mails += SystemMail(mailId, content, title, attachment, filter, publicTime.toLocalDateTime, deadline.toLocalDateTime, createTime.toLocalDateTime, updateTime.toLocalDateTime)
    }
    mails
  }

  def personalMails(playerId: Long): ListBuffer[Mail] = {
    val sql = "select mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time, sender_id, receiver_id from personal_mail where receiver_id = ?"
    val rs = DBHelper.query(sql, playerId)
    val mails: ListBuffer[Mail] = ListBuffer()
    while (rs.next()) {
      val mailId = rs.getLong("mail_id")
      val content = rs.getString("content")
      val title = rs.getString("title")
      val attachment = rs.getString("attachment")
      val filter = rs.getString("filter")
      val publicTime = rs.getTimestamp("public_time")
      val deadline = rs.getTimestamp("deadline")
      val createTime = rs.getTimestamp("create_time")
      val updateTime = rs.getTimestamp("update_time")
      val senderId = rs.getLong("sender_id")
      val receiverId = rs.getLong("receiver_id")
      if (publicTime == null || deadline == null || createTime == null || updateTime == null) {
        throw new Exception(s"Invalid mail, time is null, check the mail: $mailId in personal_mail")
      }
      mails += PersonalMail(mailId, content, title, attachment, filter, publicTime.toLocalDateTime, deadline.toLocalDateTime, createTime.toLocalDateTime, updateTime.toLocalDateTime, senderId, receiverId)
    }
    mails
  }

  def mails(playerId: Long): ListBuffer[Mail] = {
    systemMails() ++ personalMails(playerId)
  }

  def sendMail(mail: PersonalMail): Unit = {
    val mailId = snowflakeIdGenerator.nextId()
    val sql = "insert into personal_mail (mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time, sender_id, receiver_id) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    DBHelper.add(sql, mailId, mail.content, mail.title, mail.attachment, mail.filter, LocalDateTime.now, LocalDateTime.now.plusMonths(1), LocalDateTime.now, LocalDateTime.now, mail.senderId, mail.receiverId)
  }

  def delMail(mailId: Long): Unit = {
    val sql = "delete from personal_mail where mail_id = ?"
    DBHelper.delete(sql, mailId)
  }

  def addSystemMail(mail: SystemMail): Unit = {
    val mailId = snowflakeIdGenerator.nextId()
    val sql = "insert into system_mail (mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time) values (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    DBHelper.add(sql, mailId, mail.content, mail.title, mail.attachment, mail.filter, LocalDateTime.now, LocalDateTime.now.plusMonths(1), LocalDateTime.now, LocalDateTime.now)
  }

  def delSystemMail(mailId: Long): Unit = {
    val sql = "delete from system_mail where mail_id = ?"
    DBHelper.delete(sql, mailId)
  }
}
