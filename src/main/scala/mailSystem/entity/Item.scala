package mailSystem.entity

class Item {
  private var itemId: Long = _
  private var typeId: Long = _
  private var name: String = _
  private var description: String = _

  def this(itemId: Long, typeId: Long, name: String, description: String) {
    this()
    this.itemId = itemId
    this.typeId = typeId
    this.name = name
    this.description = description
  }
}
