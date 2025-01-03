package mailSystem.service

import mailSystem.dao.DBHelper
import mailSystem.entity.Item
import mailSystem.utils.SnowflakeIdGenerator

import scala.collection.mutable.ArrayBuffer

object ItemService {
  private val snowflakeIdGenerator = new SnowflakeIdGenerator(0, 0)
  private final val tableName = "item"

  def addItem(name: String, description: String, typeCode: Int): Unit = {
    require(name != null && name.nonEmpty, "物品名称不能为空")
    require(description != null && description.nonEmpty, "物品描述不能为空")
    require(typeCode > 0, "物品类型不能为空")

    val itemId = snowflakeIdGenerator.nextId()
    val sql = s"insert into $tableName (item_id, description, name, type_id) values (?, ?, ?, ?)"
    DBHelper.add(sql, itemId, description, name, typeCode)
  }

  def items(): ArrayBuffer[Item] = {
    val sql = s"select item_id, name, description, type_id from $tableName"
    val items: ArrayBuffer[Item] = ArrayBuffer()
    val rs = DBHelper.query(sql)
    try {
      while (rs._1.next()) {
        val itemId = rs._1.getLong("item_id")
        val name = rs._1.getString("name")
        val description = rs._1.getString("description")
        val typeId = rs._1.getLong("type_id")
        items += Item(itemId, typeId, name, description)
      }
    } finally {
      DBHelper.closeRsConn(rs)
    }
    items
  }

}
