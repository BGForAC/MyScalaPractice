package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.{Mail, PersonalMail, SystemMail}
import mailSystem.utils.SnowflakeIdGenerator

import java.sql.Connection
import java.time.LocalDateTime
import scala.collection.mutable.ListBuffer

/**
 * 这个类负责提供接口进行邮件的增删改查，和简单的检查
 */
object MailService {
  // 不太想用常量，但是全换final感觉也不太好
  private final val SYSTEMMAILWORKID = 16
  private val snowflakeIdGeneratorForPersonalMail = new SnowflakeIdGenerator(0, 0)
  private val snowflakeIdGeneratorForSystemMail = new SnowflakeIdGenerator(0, SYSTEMMAILWORKID)
  private val tableNameForPersonalMail = "personal_mail"
  private val tableNameForSystemMail = "system_mail"
  // 后续会使用接口实现隔离，又或者没有隔离的需求
  private val tableNameForPlayer = PlayerService.tableName
  private val tableNameForMailDel = "mail_del"

  // 判断邮件是否是系统邮件
  private def isSystemMail(mailId: Long): Boolean = {
    val datacenterIdBits = snowflakeIdGeneratorForPersonalMail.datacenterIdBits
    val workerIdBits = snowflakeIdGeneratorForPersonalMail.workerIdBits
    val sequenceBits = snowflakeIdGeneratorForPersonalMail.sequenceBits
    val mask = (1L << datacenterIdBits + workerIdBits) - 1
    ((mailId >> sequenceBits) & mask) >= (SYSTEMMAILWORKID << datacenterIdBits)
  }

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
      DBHelper.addWithConnection(sql1, personalMail.getMailId, personalMail.getContent, personalMail.getTitle, personalMail.getAttachment, personalMail.getFilter, personalMail.getPublicTime, personalMail.getDeadline, personalMail.getCreateTime, personalMail.getUpdateTime, personalMail.getSenderId, personalMail.getReceiverId)(connection)
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

  // 更新邮件领取时间，只更新用户收到的邮件，系统邮件暂时无法处理，需要额外加表
  def updateMailUpdateTimeInterface(mailId: Long)(connection: Connection): Unit = {
    if (isSystemMail(mailId)) return
    val sql = s"update $tableNameForPersonalMail set update_time = ? where mail_id = ?"
    val time = LocalDateTime.now
    DBHelper.updateWithConnection(sql, time, mailId)(connection)
  }


//  // 真正意义上的删邮件
//  def delSystemMail(mailId: Long): Unit = {
//    val sql = s"delete from $tableNameForSystemMail where mail_id = ?"
//    DBHelper.delete(sql, mailId)
//  }

  /**
   * 获取邮件。根据id可以直接判断是哪个类型的邮件
   */
  def getMail(mailId: Long): Mail = {
    if (isSystemMail(mailId)) {
      val sql = s"select mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time from $tableNameForSystemMail where mail_id = ?"
      val rs = DBHelper.query(sql, mailId)
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
          new SystemMail(mailId, content, title, attachment, filter, publicTime.toLocalDateTime, deadline.toLocalDateTime, createTime.toLocalDateTime, updateTime.toLocalDateTime)
        } else {
          throw new Exception(s"邮件不存在: $mailId")
        }
      } finally {
        DBHelper.closeRsConn(rs)
      }
    } else {
      val sql = s"select mail_id, content, title, attachment, filter, public_time, deadline, create_time, update_time, sender_id, receiver_id from $tableNameForPersonalMail where mail_id = ?"
      val rs = DBHelper.query(sql, mailId)
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
          new PersonalMail(mailId, content, title, attachment, filter, publicTime.toLocalDateTime, deadline.toLocalDateTime, createTime.toLocalDateTime, updateTime.toLocalDateTime, senderId, receiverId)
        } else {
          throw new Exception(s"邮件不存在: $mailId")
        }
      } finally {
        DBHelper.closeRsConn(rs)
      }
    }
  }

  // 客户端领取附件时，服务端通过邮件ID获取附件，防止客户端篡改附件数据
  def getAttachment(mailId: Long): String = {
    if (isSystemMail(mailId)) {
      val sql = s"select attachment from $tableNameForSystemMail where mail_id = ?"
      val rs = DBHelper.query(sql, mailId)
      try {
        if (rs._1.next()) rs._1.getString("attachment") else ""
      } finally {
        DBHelper.closeRsConn(rs)
      }
    } else {
      val sql = s"select attachment from $tableNameForPersonalMail where mail_id = ?"
      val rs = DBHelper.query(sql, mailId)
      try {
        if (rs._1.next()) rs._1.getString("attachment") else ""
      } finally {
        DBHelper.closeRsConn(rs)
      }
    }

  }
}
