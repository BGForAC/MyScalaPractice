package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.{Mail, Player}
import mailSystem.utils.{MapBean, SnowflakeIdGenerator}

import scala.collection.mutable.ArrayBuffer

object PlayerService {
  private val snowflakeIdGenerator = new SnowflakeIdGenerator(0, 0)
  private final val tableName = "player"
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

  def players(): ArrayBuffer[Player] = {
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

  def collectAttachment(playerId: Long, mail: Mail): Unit = {
    val attachment = MapBean.toMutableMap(mail.getAttachment).toMap.asInstanceOf[Map[String, Int]]
    if (attachment.isEmpty) throw new Exception(s"邮件 ${mail.getMailId} 没有附件")
    //    if (attachment.isEmpty) println(s"邮件 ${mail.getMailId} 没有附件")
    if (player.getMailsCollect.contains(mail.getMailId.toString)) throw new Exception(s"玩家 ${player.getName} 已经领取过邮件 ${mail.getMailId} 的附件")
    //    if (player.getMailsCollect.contains(mail.getMailId.toString)) println(s"玩家 ${player.getName} 已经领取过邮件 ${mail.getMailId} 的附件")

//    attachment.foreach(println)
    val sql1 = s"update $tableName set mails_collect = concat(coalesce(mails_collect), ?) where player_id = ?"
    val sql2 = s"insert into $tableNameForItem (player_id, item_id, quantity) values (?, ?, ?)" +
      s"on duplicate key update quantity = quantity + values(quantity)"
    val para1 = Seq(sql1, s"${mail.getMailId},", playerId)
    val para2 = attachment.map { case (itemId, quantity) => Seq(sql2, playerId, itemId.toLong, quantity) }.toSeq
    val paras = Seq(para1) ++ para2
    DBHelper.atomicUpdate(paras)
  }

  def collectAttachment(player: Player, mail: Mail): Unit = {
    collectAttachment(player.getPlayerId, mail)
    println(s"玩家 ${player.getName} 领取了邮件 ${mail.getMailId} 的附件")
  }
}
