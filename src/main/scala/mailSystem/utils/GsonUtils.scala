package mailSystem.utils

import com.google.gson.Gson

object GsonUtils {

  def gson = new Gson()

  def map2Json(map: Map[String, Any]): String = {
    gson.toJson(map)
  }

}
