package mailSystem.entity

import java.util.Date

class SystemMail(var id: Int, var content: String, var title: String, var attachment: String, var filter: String, var deliveryTime: Date, var deadline: Date) {
  override def toString: String = {
    s"Sysmail(id=$id, content=$content, title=$title, attachment=$attachment, filter=$filter, deliveryTime=$deliveryTime, deadline=$deadline)"
  }
}
