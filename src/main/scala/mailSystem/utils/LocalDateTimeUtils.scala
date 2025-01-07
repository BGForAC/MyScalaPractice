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
      val hour = time.split(":")(0).toInt
      val minute = time.split(":")(1).toInt
      val second = time.split(":")(2).toInt
      LocalDateTime.of(year, month, day, hour, minute, second)
    } catch {
      case e: Exception =>
        null
    }
  }
}
