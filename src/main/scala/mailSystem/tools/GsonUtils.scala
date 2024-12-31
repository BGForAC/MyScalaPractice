package mailSystem.tools

import com.google.gson.Gson

object GsonUtils {
  private val gson = new Gson()

  def strMapToJson(str: Map[String, String]): String = {
    gson.toJson(str)
  }

  def strMapFromJson(str: String): Map[String, String] = {
    gson.fromJson(str, classOf[Map[String, String]])
  }

  def recMapFromJson(str: String): Map[String, Map[String, String]] = {
    gson.fromJson(str, classOf[Map[String, Map[String, String]]])
  }

  def recMapToJson(str: Map[String, Map[String, String]]): String = {
    gson.toJson(str)
  }
}
