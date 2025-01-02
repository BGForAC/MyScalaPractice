package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.Player
import mailSystem.utils.SnowflakeIdGenerator

import scala.collection.mutable.ArrayBuffer

object PlayerService {
  private val snowflakeIdGenerator = new SnowflakeIdGenerator(0, 0)

  def addPlayer(name: String): Unit = {
    val playerId = snowflakeIdGenerator.nextId()
    val sql = "insert into player (player_id, name) values (?, ?)"
    DBHelper.add(sql, playerId, name)
  }

  def delPlayer(playerId: Long): Unit = {
    val sql = "delete from player where player_id = ?"
    DBHelper.delete(sql, playerId)
  }

  def players(): ArrayBuffer[Player] = {
    val sql = "select * from player"
    val rs = DBHelper.query(sql)
    val players: ArrayBuffer[Player] = ArrayBuffer()
    while (rs.next()) {
      val playerId = rs.getLong("player_id")
      val name = rs.getString("name")
      players += Player(playerId, name)
    }
    players
  }
}
