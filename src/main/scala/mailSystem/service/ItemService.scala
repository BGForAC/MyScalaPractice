package mailSystem.service

import mailSystem.entity.Item
import mailSystem.utils.{DBHelper, SnowflakeIdGenerator}

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

  def getItem(itemId: Long): Item = {
    val sql = s"select item_id, name, description, type_id from $tableName where item_id = ?"
    val rs = DBHelper.query(sql, itemId)
    try {
      if (rs._1.next()) {
        val name = rs._1.getString("name")
        val description = rs._1.getString("description")
        val typeId = rs._1.getLong("type_id")
        Item(itemId, typeId, name, description)
      } else {
        throw new Exception(s"没有找到物品 $itemId")
      }
    } finally {
      DBHelper.closeRsConn(rs)
    }
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
