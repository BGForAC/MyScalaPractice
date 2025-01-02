package mailSystem.entity

case class Player(
  private var playerId: Long,
  private var name: String
) {
  def getPlayerId: Long = playerId
  def getName: String = name
}
