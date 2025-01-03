package mailSystem.utils

object MapBeanUtils {

  def map2Json(map: Map[String, Any]): String = {
    var mapBean: MapBean = MapBean.empty
    map.foreach{mapBean += _}
    mapBean.toJsonString
  }

}
