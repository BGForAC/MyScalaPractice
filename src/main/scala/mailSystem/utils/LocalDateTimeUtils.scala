package mailSystem.utils

import java.time.LocalDateTime

object LocalDateTimeUtils {
  def string2Time(timeString: String): LocalDateTime = {
    try {
      val date = timeString.split("T")(0)
      val time = timeString.split("T")(1)
      val year = date.split("-")(0).toInt
      val month = date.split("-")(1).toInt
      val day = date.split("-")(2).toInt
      time.split(":") match {
        case Array(hour, minute, second) =>
          LocalDateTime.of(year, month, day, hour.toInt, minute.toInt, second.toInt)
        case Array(hour, minute) =>
          LocalDateTime.of(year, month, day, hour.toInt, minute.toInt)
      }
    } catch {
      case e: Exception =>
        null
    }
  }
}
