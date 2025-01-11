package mailSystem.entity

case class Item(
  private var itemId: Long,
  private var typeId: Long,
  private var name: String,
  private var description: String
) {
  def getItemId: Long = itemId
  def getTypeId: Long = typeId
  def getName: String = name
  def getDescription: String = description

  override def equals(obj: Any): Boolean = {
    obj match {
      case item: Item => itemId == item.itemId
      case _ => false
    }
  }

  override def hashCode(): Int = {
    itemId.hashCode()
  }

  // json格式
  override def toString: String = {
    s"""{"itemId":$itemId,"typeId":$typeId,"name":"$name","description":"$description"}"""
  }
}
