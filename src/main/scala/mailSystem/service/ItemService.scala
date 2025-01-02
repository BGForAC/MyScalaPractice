package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.Item
import mailSystem.utils.SnowflakeIdGenerator

import scala.collection.mutable.ArrayBuffer

object ItemService {
  private val snowflakeIdGenerator = new SnowflakeIdGenerator(0, 0)

  def addItem(name: String, description: String, typeCode: Long): Unit = {
    val itemId = snowflakeIdGenerator.nextId()
    val sql = "insert into item (item_id, description, name, type_id) values (?, ?, ?, ?)"
    println(s"itemId: $itemId, description: $description, name: $name, typeCode: $typeCode")
    DBHelper.add(sql, itemId, description, name, typeCode)
  }

  def items(): ArrayBuffer[Item] = {
    val sql = "select * from item"
    val rs = DBHelper.query(sql)
    val items: ArrayBuffer[Item] = ArrayBuffer()
    while (rs.next()) {
      val itemId = rs.getLong("item_id")
      val name = rs.getString("name")
      val description = rs.getString("description")
      val typeId = rs.getLong("type_id")
      items += Item(itemId, typeId, name, description)
    }
    items
  }

}
