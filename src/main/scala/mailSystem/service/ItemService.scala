package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.utils.SnowflakeIdGenerator

object ItemService {
  private val snowflakeIdGenerator = new SnowflakeIdGenerator(0, 0)

  def addItem(name: String, description: String, typeCode: Long): Unit = {
    val itemId = snowflakeIdGenerator.nextId()
    val sql = "insert into item (item_id, description, name, type_id) values (?, ?, ?, ?)"
    println(s"itemId: $itemId, description: $description, name: $name, typeCode: $typeCode")
    DBHelper.add(sql, itemId, description, name, typeCode)
  }

}
