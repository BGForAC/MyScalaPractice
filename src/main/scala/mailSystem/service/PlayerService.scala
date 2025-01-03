package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.Player
import mailSystem.utils.SnowflakeIdGenerator

import scala.collection.mutable.ArrayBuffer

object PlayerService {
  private val snowflakeIdGenerator = new SnowflakeIdGenerator(0, 0)
  private final val tableName = "player"

  def addPlayer(name: String): Unit = {
    val playerId = snowflakeIdGenerator.nextId()
    val sql = s"insert into $tableName (player_id, name) values (?, ?)"
    DBHelper.add(sql, playerId, name)
  }

  def delPlayer(playerId: Long): Unit = {
    val sql = s"delete from $tableName where player_id = ?"
    DBHelper.delete(sql, playerId)
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
      rs._2.close()
    }
  }

  def readMail(player: Player, mailId: Long): Unit = {
    val sql = s"update $tableName set mails_read = concat(coalesce(mails_read), ?) where player_id = ?"
    if (player.getMailsRead.contains(mailId.toString)) {
      println(s"玩家 ${player.getName} 已经阅读过邮件 $mailId")
    } else {
      DBHelper.update(sql, s"${mailId.toString},", player.getPlayerId)
    }
  }

  def collectAttachment(playerId: Long, mailId: Long): Unit = {
    val sql = s"update $tableName set mails_collect = concat(coalesce(mails_collect), ?) where player_id = ?"
    DBHelper.update(sql, s"${mailId.toString},", playerId)
  }
}
