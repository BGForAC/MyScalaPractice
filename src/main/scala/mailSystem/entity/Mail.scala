package mailSystem.entity

trait Mail {
  var id: Int
  var content: String
  var title: String
  var attachment: String
  var filter: String
  var deliveryTime: java.util.Date
  var deadline: java.util.Date

  override def toString: String = {
    s"Mail(id=$id, content=$content, title=$title, attachment=$attachment, filter=$filter, deliveryTime=$deliveryTime, deadline=$deadline)"
  }
}
