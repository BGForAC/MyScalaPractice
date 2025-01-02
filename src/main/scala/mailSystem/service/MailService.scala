package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.utils.{GsonUtils, SnowflakeIdGenerator}

import scala.collection.mutable.ListBuffer

object MailService {
  private val snowflakeIdGenerator = new SnowflakeIdGenerator(0, 0)

  def systemMails(): ListBuffer[Mail] = {
    val sql = "select * from system_mail"
    val rs = DBHelper.query(sql)
    var mails: ListBuffer[Mail] = ListBuffer()
    while (rs.next()) {
      val mailId = rs.getLong("mail_id")
      val content = rs.getString("content")
      val title = rs.getString("title")
      val attachment = rs.getString("attachment")
      val filter = rs.getString("filter")
      val publicTime = rs.getDate("public_time")
      val deadline = rs.getDate("deadline")
      val createTime = rs.getDate("create_time")
      val updateTime = rs.getDate("update_time")
      mails += new SystemMail(mailId, content, title, attachment, filter, publicTime, deadline, createTime, updateTime)
//      mails += new SystemMail(mailId, senderId, receiverId, title, content, attachment)
    }
    mails
  }

  def personalMails(playerId: Long): ListBuffer[Mail] = {
    val sql = "select * from personal_mail where receiver = ?"
    val rs = DBHelper.query(sql, playerId)
    var mails: ListBuffer[Mail] = ListBuffer()
    while (rs.next()) {
      val mailId = rs.getLong("id")
      val senderId = rs.getLong("sender_id")
      val title = rs.getString("title")
      val content = rs.getString("content")
      val attachment = rs.getString("attachment")
//      mails += new PersonalMail(mailId, senderId, playerId, title, content, attachment)
    }
    mails
  }

  def mails(playerId: Long): ListBuffer[Mail] = {
    systemMails() ++ personalMails(playerId)
  }

  def sendMail(senderId: Long, receiverId: Long, title: String, content: String, attachment: String): Unit = {
    val sql = "insert into personal_mail (sender_id, receiver_id, title, content, attachment) values (?, ?, ?, ?, ?)"
    DBHelper.add(sql, senderId, receiverId, title, content, attachment)
  }

  def delMail(mailId: Long): Unit = {
    val sql = "delete from personal_mail where mail_id = ?"
    DBHelper.delete(sql, mailId)
  }

  def addSystemMail(senderId: Long, receiverId: Long, title: String, content: String, attachment: String): Unit = {
    val mailId = snowflakeIdGenerator.nextId()
    val sql = "insert into system_mail (id, sender_id, receiver_id, title, content, attachment) values (?, ?, ?, ?, ?, ?)"
    DBHelper.add(sql, mailId, senderId, receiverId, title, content, attachment)
  }

  def delSystemMail(mailId: Long): Unit = {
    val sql = "delete from system_mail where mail_id = ?"
    DBHelper.delete(sql, mailId)
  }



}
