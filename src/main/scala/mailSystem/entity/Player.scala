package mailSystem.entity

class Player {
  private var playerId: Long = _
  private var name: String = _

  def this(playerId: Long, name: String, mail_read: String, mail_receive: String) {
    this()
    this.playerId = playerId
    this.name = name
  }
}
