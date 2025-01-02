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
}
