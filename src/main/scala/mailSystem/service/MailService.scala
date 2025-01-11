package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.utils.{SnowflakeIdGenerator}

import java.time.LocalDateTime
import scala.collection.mutable.ListBuffer

/**
 * 这个类负责提供接口进行邮件的增删改查，和简单的检查
 */
object MailService {
  private val snowflakeIdGeneratorForPersonalMail = new SnowflakeIdGenerator(0, 0)
  private val snowflakeIdGeneratorForSystemMail = new SnowflakeIdGenerator(0, 16)
  private val tableNameForPersonalMail = "personal_mail"
  private val tableNameForSystemMail = "system_mail"
  private val tableNameForPlayer = PlayerService.tableName
  private val tableNameForMailDel = "mail_del"

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
    val sql = s"select mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time, sender_id, receiver_id from $tableNameForPersonalMail where receiver_id = ? or sender_id = ?"
    val mails: ListBuffer[Mail] = ListBuffer()
    val rs = DBHelper.query(sql, playerId, playerId)
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

  def deletedMails(playerId: Long): ListBuffer[Mail] = {
    val sql = s"select mail_id from $tableNameForMailDel where player_id = ?"
    val mails: ListBuffer[Mail] = ListBuffer()
    val rs = DBHelper.query(sql, playerId)
    try {
      while (rs._1.next()) {
        val mailId = rs._1.getLong("mail_id")
        val mail = getMail(mailId)
        mails += mail
      }
    } finally {
      DBHelper.closeRsConn(rs)
    }
    mails
  }

  def mails(playerId: Long): ListBuffer[Mail] = {
    systemMails() ++ personalMails(playerId)
  }

  // 待修改：目前就先用同样的参数，每次要改两遍，后面再改
  def addPersonalMail(senderId: Long, receiverId: Long, mail: PersonalMail): PersonalMail = {
    require(senderId > 0, "发件人不能为空")
    require(receiverId > 0, "收件人不能为空")
    require(senderId != receiverId, "发件人和收件人不能相同")
    require(mail.title != null && mail.title.nonEmpty, "邮件标题不能为空")
    require(mail.content != null && mail.content.nonEmpty, "邮件内容不能为空")

    val mailId = snowflakeIdGeneratorForPersonalMail.nextId()
    val sql1 = s"insert into $tableNameForPersonalMail (mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time, sender_id, receiver_id) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    val sql2 = s"update $tableNameForPlayer set mail_count = mail_count + 1 where player_id = ?"
    val time = LocalDateTime.now
    val personalMail = new PersonalMail(mailId, mail.content, mail.title, mail.attachment, mail.filter, time, time.plusMonths(1), time, time, mail.senderId, mail.receiverId)

    DBHelper.atomicOperation{ connection =>
      DBHelper.addWithConnection(sql1, mailId, mail.content, mail.title, mail.attachment, mail.filter, time, time.plusMonths(1), time, time, mail.senderId, mail.receiverId)(connection)
      DBHelper.updateWithConnection(sql2, mail.receiverId)(connection)
    }
    personalMail
  }

  // 对自己发送的邮件在一个删除表中添加新行
  def deleteMailSend(playerId: Long, mailId: Long): Unit = {
    val sql = s"insert into $tableNameForMailDel (player_id, mail_id, delete_time) values (?, ?, ?)"
    DBHelper.add(sql, playerId, mailId, LocalDateTime.now)
  }

  // 对系统邮件在删除表中添加新行，并且更新阅读和领取状态
  def deleteSystemMail(playerId: Long, mailId: Long): Unit = {
    val sql1 = s"insert into $tableNameForMailDel (player_id, mail_id, delete_time) values (?, ?, ?)"
    val sql2 = s"update $tableNameForPlayer set mails_collect = replace(mails_collect, '$mailId,', '')," +
                                              s"mails_read = replace(mails_read, '$mailId,', '') where player_id = ?"
    DBHelper.atomicOperation{ connection =>
      DBHelper.addWithConnection(sql1, playerId, mailId, LocalDateTime.now)(connection)
      DBHelper.updateWithConnection(sql2, playerId)(connection)
    }
  }

  // 对收到的邮件，在一个删除表中添加新行，还需要更新玩家的邮件数量和阅读与领取状态
  def deleteMailReceive(playerId: Long, mailId: Long): Unit = {
    val sql1 = s"insert into $tableNameForMailDel (player_id, mail_id, delete_time) values (?, ?, ?)"
    val sql2 = s"update $tableNameForPlayer set mail_count = mail_count - 1," +
                                              s"mails_collect = replace(mails_collect, '$mailId,', '')," +
                                              s"mails_read = replace(mails_read, '$mailId,', '') where player_id = ?"
    DBHelper.atomicOperation{ connection =>
      DBHelper.addWithConnection(sql1, playerId, mailId, LocalDateTime.now)(connection)
      DBHelper.updateWithConnection(sql2, playerId)(connection)
    }
  }

  /**
   * 添加系统邮件
   */
  def addSystemMail(mail: SystemMail): SystemMail = {
    require(mail.title != null && mail.title.nonEmpty, "邮件标题不能为空")
    require(mail.content != null && mail.content.nonEmpty, "邮件内容不能为空")

    val mailId = snowflakeIdGeneratorForSystemMail.nextId()
    val sql = s"insert into $tableNameForSystemMail (mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time) values (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    val time = LocalDateTime.now
    val systemMail = new SystemMail(mailId, mail.content, mail.title, mail.attachment, mail.filter, time, time.plusMonths(1), time, time)
    DBHelper.add(sql, systemMail.getMailId, systemMail.getContent, systemMail.getTitle, systemMail.getAttachment, systemMail.getFilter, systemMail.getPublicTime, systemMail.getPublicTime.plusMonths(1), systemMail.getCreateTime, systemMail.getUpdateTime)
    systemMail
  }


//  // 真正意义上的删邮件
//  def delSystemMail(mailId: Long): Unit = {
//    val sql = s"delete from $tableNameForSystemMail where mail_id = ?"
//    DBHelper.delete(sql, mailId)
//  }

  /**
   * 获取邮件
   */
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
