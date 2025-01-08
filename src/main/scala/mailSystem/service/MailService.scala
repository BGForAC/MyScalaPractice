package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.utils.{MapBean, SnowflakeIdGenerator}

import java.time.LocalDateTime
import scala.collection.mutable.ListBuffer

object MailService {
  private val snowflakeIdGeneratorForPersonalMail = new SnowflakeIdGenerator(0, 0)
  private val snowflakeIdGeneratorForSystemMail = new SnowflakeIdGenerator(0, 16)
  private val tableNameForPersonalMail = "personal_mail"
  private val tableNameForSystemMail = "system_mail"

  def systemMails(): ListBuffer[Mail] = {
    val sql = s"select mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time from $tableNameForSystemMail"
    val mails: ListBuffer[Mail] = ListBuffer()
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
          throw new Exception(s"非法的邮箱，时间为空，请检查邮箱: $mailId in system_mail")
        }
        mails += new SystemMail(mailId, content, title, attachment, filter, publicTime.toLocalDateTime, deadline.toLocalDateTime, createTime.toLocalDateTime, updateTime.toLocalDateTime)
      }
    } finally {
      DBHelper.closeRsConn(rs)
    }
    mails
  }

  def personalMails(playerId: Long): ListBuffer[Mail] = {
    val sql = s"select mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time, sender_id, receiver_id from $tableNameForPersonalMail where receiver_id = ?"
    val mails: ListBuffer[Mail] = ListBuffer()
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
          throw new Exception(s"非法的邮箱，时间为空，请检查邮箱: $mailId in personal_mail")
        }
        mails += new PersonalMail(mailId, content, title, attachment, filter, publicTime.toLocalDateTime, deadline.toLocalDateTime, createTime.toLocalDateTime, updateTime.toLocalDateTime, senderId, receiverId)
      }
    } finally {
      DBHelper.closeRsConn(rs)
    }
    mails
  }

  def mails(playerId: Long): ListBuffer[Mail] = {
    systemMails() ++ personalMails(playerId)
  }

  def addPersonalMail(senderId: Long, receiverId: Long, mail: PersonalMail): Unit = {
    require(senderId > 0, "发件人不能为空")
    require(receiverId > 0, "收件人不能为空")
    require(senderId != receiverId, "发件人和收件人不能相同")
    require(mail.title != null && mail.title.nonEmpty, "邮件标题不能为空")
    require(mail.content != null && mail.content.nonEmpty, "邮件内容不能为空")
//    require(MapBean.toMutableMap(mail.filter) != MapBean.empty, "邮件过滤条件不能为空")

    val mailId = snowflakeIdGeneratorForPersonalMail.nextId()
    val sql1 = s"insert into $tableNameForPersonalMail (mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time, sender_id, receiver_id) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    val sql2 = s"update player set mail_count = mail_count + 1 where player_id = ?"
    val time = LocalDateTime.now

    DBHelper.atomicOperation{ connection =>
      DBHelper.addWithConnection(sql1, mailId, mail.content, mail.title, mail.attachment, mail.filter, time, time.plusMonths(1), time, time, mail.senderId, mail.receiverId)(connection)
      DBHelper.updateWithConnection(sql2, mail.receiverId)(connection)
    }
  }

  // 在一个删除表中添加新行，若是私人邮件且自己是收件人，则删除邮件时，需要更新玩家的邮件数量
  def delMail(playerId: Long, mailId: Long): Unit = {
    val sql = s"insert into mail_del (player_id, mail_id) values (?, ?)"
    getMail(mailId) match {
      case mail: SystemMail =>
        DBHelper.add(sql, playerId, mailId)
      case mail: PersonalMail if mail.senderId == playerId =>
        DBHelper.add(sql, playerId, mailId)
      case mail: PersonalMail if mail.receiverId == playerId =>
        val sql2 = s"update player set mail_count = mail_count - 1 where player_id = ?"
        DBHelper.atomicOperation{ connection =>
          DBHelper.addWithConnection(sql, playerId, mailId)(connection)
          DBHelper.updateWithConnection(sql2, playerId)(connection)
        }
      case _ =>
        throw new Exception(s"非法操作, $playerId 尝试删除不属于自己邮箱的邮件 $mailId")
    }

  }

  def addSystemMail(mail: SystemMail): Unit = {
    require(mail.title != null && mail.title.nonEmpty, "邮件标题不能为空")
    require(mail.content != null && mail.content.nonEmpty, "邮件内容不能为空")
//    require(MapBean.toMutableMap(mail.filter) != MapBean.empty, "邮件过滤条件不能为空")

    val mailId = snowflakeIdGeneratorForSystemMail.nextId()
    val sql = s"insert into $tableNameForSystemMail (mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time) values (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    val time = LocalDateTime.now
    DBHelper.add(sql, mailId, mail.content, mail.title, mail.attachment, mail.filter, time, time.plusMonths(1), time, time)
  }

  def delSystemMail(mailId: Long): Unit = {
    val sql = s"delete from $tableNameForSystemMail where mail_id = ?"
    DBHelper.delete(sql, mailId)
  }

  def getMail(mailId: Long): Mail = {
    var mail: Mail = null
    val sql1 = s"select mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time, sender_id, receiver_id from $tableNameForPersonalMail where mail_id = ?"
    val rs = DBHelper.query(sql1, mailId)
    try {
      if (rs._1.next()) {
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
          throw new Exception(s"非法的邮箱，时间为空，请检查邮箱: $mailId in personal_mail")
        }
        mail = new PersonalMail(mailId, content, title, attachment, filter, publicTime.toLocalDateTime, deadline.toLocalDateTime, createTime.toLocalDateTime, updateTime.toLocalDateTime, senderId, receiverId)
      }
    } finally {
      DBHelper.closeRsConn(rs)
    }
    if (mail == null) {
      val sql2 = s"select mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time from $tableNameForSystemMail where mail_id = ?"
      val rs = DBHelper.query(sql2, mailId)
      try {
        if (rs._1.next()) {
          val content = rs._1.getString("content")
          val title = rs._1.getString("title")
          val attachment = rs._1.getString("attachment")
          val filter = rs._1.getString("filter")
          val publicTime = rs._1.getTimestamp("public_time")
          val deadline = rs._1.getTimestamp("deadline")
          val createTime = rs._1.getTimestamp("create_time")
          val updateTime = rs._1.getTimestamp("update_time")
          if (publicTime == null || deadline == null || createTime == null || updateTime == null) {
            throw new Exception(s"非法的邮箱，时间为空，请检查邮箱: $mailId in system_mail")
          }
          mail = new SystemMail(mailId, content, title, attachment, filter, publicTime.toLocalDateTime, deadline.toLocalDateTime, createTime.toLocalDateTime, updateTime.toLocalDateTime)
        }
      } finally {
        DBHelper.closeRsConn(rs)
      }
    }
    mail
  }

  // 客户端领取附件时，服务端通过邮件ID获取附件，防止客户端篡改附件数据
  def getAttachment(mailId: Long): String = {
    var attachment = ""
    val sql1 = s"select attachment from $tableNameForPersonalMail where mail_id = ?"
    val rs = DBHelper.query(sql1, mailId)
    try {
      attachment = if (rs._1.next()) rs._1.getString("attachment") else ""
    } finally {
      DBHelper.closeRsConn(rs)
    }
    if (attachment.isEmpty) {
      val sql2 = s"select attachment from $tableNameForSystemMail where mail_id = ?"
      val rs = DBHelper.query(sql2, mailId)
      try {
        attachment = if (rs._1.next()) rs._1.getString("attachment") else ""
      } finally {
        DBHelper.closeRsConn(rs)
      }
    }
    attachment
  }
}
