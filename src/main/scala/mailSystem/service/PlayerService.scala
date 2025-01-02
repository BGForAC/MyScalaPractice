package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.utils.{GsonUtils, SnowflakeIdGenerator}

object PlayerService {
  private val snowflakeIdGenerator = new SnowflakeIdGenerator(0, 0)

  def addUser(name: String): Unit = {
    val playerId = snowflakeIdGenerator.nextId()
    val sql = "insert into player (player_id, name) values (?, ?)"
    DBHelper.add(sql, playerId, name)
  }

  def delUser(playerId: Long): Unit = {
    val sql = "delete from player where player_id = ?"
    DBHelper.delete(sql, playerId)
  }
}
