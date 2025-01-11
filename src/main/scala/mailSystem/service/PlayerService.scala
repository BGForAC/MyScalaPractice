package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.{Mail, Player}
import mailSystem.utils.{MapBean, SnowflakeIdGenerator}

import scala.collection.mutable.ArrayBuffer

object PlayerService {
  private val snowflakeIdGenerator = new SnowflakeIdGenerator(0, 0)
  final val tableName = "player"
  private final val tableNameForItem = "player_items"

  def addPlayer(name: String): Unit = {
    require(name != null && name.nonEmpty, "玩家名称不能为空")

    val playerId = snowflakeIdGenerator.nextId()
    val sql = s"insert into $tableName (player_id, name) values (?, ?)"
    DBHelper.add(sql, playerId, name)
  }

  def delPlayer(playerId: Long): Unit = {
    val sql = s"delete from $tableName where player_id = ?"
    DBHelper.delete(sql, playerId)
  }

  def getPlayer(playerId: Long): Player = {
    val sql = s"select player_id, name, mails_read, mails_collect from $tableName where player_id = ?"
    val rs = DBHelper.query(sql, playerId)
    try {
      if (rs._1.next()) {
        val name = rs._1.getString("name")
        val mailsRead = rs._1.getString("mails_read")
        val mailsCollect = rs._1.getString("mails_collect")
        new Player(playerId, name, mailsRead, mailsCollect)
      } else {
        throw new Exception(s"没有找到玩家 $playerId")
      }
    } finally {
      DBHelper.closeRsConn(rs)
    }
  }

  def getCollectStatus(playerId: Long): String = {
    val sql = s"select mails_collect from $tableName where player_id = ?"
    val rs = DBHelper.query(sql, playerId)
    try {
      if (rs._1.next()) rs._1.getString("mails_collect") else ""
    } finally {
      DBHelper.closeRsConn(rs)
    }
  }

  def getReadStatus(playerId: Long): String = {
    val sql = s"select mails_read from $tableName where player_id = ?"
    val rs = DBHelper.query(sql, playerId)
    try {
      if (rs._1.next()) rs._1.getString("mails_read") else ""
    } finally {
      DBHelper.closeRsConn(rs)
    }
  }

  def getMailCount(playerId: Long): Int = {
    val sql = s"select mail_count from $tableName where player_id = ?"
    val rs = DBHelper.query(sql, playerId)
    try {
      if (rs._1.next()) rs._1.getInt("mail_count") else throw new Exception(s"没有找到玩家 $playerId")
    } finally {
      DBHelper.closeRsConn(rs)
    }
  }

  def IncMailCountByOne(playerId: Long): Unit = {
    val sql = s"update $tableName set mail_count = mail_count + 1 where player_id = ?"
    DBHelper.update(sql, playerId)
  }

  def decMailCountByOne(playerId: Long, mailCount: Int): Unit = {
    val sql = s"update $tableName set mail_count = mail_count - 1 where player_id = ?"
    DBHelper.update(sql, playerId)
  }

  def updateMailsRead(playerId: Long, mailsRead: String): Unit = {
    val sql = s"update $tableName set mails_read = ? where player_id = ?"
    DBHelper.update(sql, mailsRead, playerId)
  }

  def allIds(): ArrayBuffer[Long] = {
    val sql = s"select player_id from $tableName"
    val rs = DBHelper.query(sql)
    try {
      val playersIds: ArrayBuffer[Long] = ArrayBuffer()
      while (rs._1.next()) {
        val playerId = rs._1.getLong("player_id")
        playersIds += playerId
      }
      playersIds
    } finally {
      DBHelper.closeRsConn(rs)
    }
  }

  def allPlayers(): ArrayBuffer[Player] = {
    val sql = s"select player_id, name, mails_read, mails_collect from $tableName"
    val rs = DBHelper.query(sql)
    try {
      val players: ArrayBuffer[Player] = ArrayBuffer()
      while (rs._1.next()) {
        val playerId = rs._1.getLong("player_id")
        val name = rs._1.getString("name")
        val mailsRead = rs._1.getString("mails_read")
        val mailsCollect = rs._1.getString("mails_collect")
        players += new Player(playerId, name, mailsRead, mailsCollect)
      }
      players
    } finally {
      DBHelper.closeRsConn(rs)
    }
  }

  def getItemCount(playerId: Long, itemId: Long): Int = {
    val sql = s"select quantity from $tableNameForItem where player_id = ? and item_id = ?"
    val rs = DBHelper.query(sql, playerId, itemId)
    try {
      if (rs._1.next()) rs._1.getInt("quantity") else 0
    } finally {
      DBHelper.closeRsConn(rs)
    }
  }

  def getItems(playerId: Long): Map[Long, Int] = {
    val sql = s"select item_id, quantity from $tableNameForItem where player_id = ?"
    val rs = DBHelper.query(sql, playerId)
    try {
      var items: Map[Long, Int] = Map()
      while (rs._1.next()) {
        val itemId = rs._1.getLong("item_id")
        val quantity = rs._1.getInt("quantity")
        items += (itemId -> quantity)
      }
      items
    } finally {
      DBHelper.closeRsConn(rs)
    }
  }

  def readMail(player: Player, mailId: Long): Unit = {
    if (player.getMailsRead.contains(mailId.toString)) throw new Exception(s"玩家 ${player.getName} 已经阅读过邮件 ${mailId}")

    val sql = s"update $tableName set mails_read = concat(coalesce(mails_read), ?) where player_id = ?"
    DBHelper.update(sql, s"${mailId.toString},", player.getPlayerId)
  }

  def collectAttachment(playerId: Long, mailId: Long, attachmentJson: String): Unit = {
    val attachment = MapBean.toMutableMap(attachmentJson).toMap.asInstanceOf[Map[String, Int]]
    collectAttachment(playerId, mailId, attachment)
  }

  /**
   * 领取邮件附件
   */
  def collectAttachment(playerId: Long, mailId: Long, attachment: Map[String, Int]): Unit = {
    if (attachment.isEmpty) throw new Exception(s"邮件 ${mailId} 没有附件")
    val mailsCollect = getCollectStatus(playerId)
    if (mailsCollect.contains(mailId.toString)) throw new IllegalStateException(s"玩家 ${playerId} 已经领取过邮件 ${mailId} 的附件")

    val sql1 = s"update $tableName set mails_collect = concat(coalesce(mails_collect, ''), ?) where player_id = ?"
    val sql2 = s"insert into $tableNameForItem (player_id, item_id, quantity) values (?, ?, ?)" +
      s"on duplicate key update quantity = quantity + values(quantity)"

    DBHelper.atomicOperation{ connection =>
      DBHelper.updateWithConnection(sql1, s"$mailId,", playerId)(connection)
      attachment.foreach { case (itemId, quantity) =>
        DBHelper.addWithConnection(sql2, playerId, itemId.toLong, quantity)(connection)
      }
    }
  }

  def collectAttachment(player: Player, mail: Mail): Unit = {
    collectAttachment(player.getPlayerId, mail.getMailId, mail.getAttachment)
  }
}