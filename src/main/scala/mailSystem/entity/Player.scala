package mailSystem.entity

case class Player(
  private var playerId: Long,
  private var name: String,
  private var mailsRead: String,
  private var mailsCollect: String
) {

  def this() = {
    this(0, "", "", "")
  }

  def this(name: String) = {
    this()
    this.name = name
  }

  def this(playerId: Long, name: String) = {
    this()
    this.name = name
    this.playerId = playerId
  }

  def getPlayerId: Long = playerId
  def getName: String = name
  def getMailsRead: String = mailsRead
  def getMailsCollect: String = mailsCollect
}
