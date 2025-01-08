package mailSystem.utils

import mailSystem.entity.SystemMail

import java.time.LocalDateTime

object MapBeanUtils {

  def map2Json(map: Map[String, Any]): String = {
    var mapBean: MapBean = MapBean.empty
    map.foreach{mapBean += _}
    mapBean.toJsonString
  }

  def json2SystemMail(mailJsonString: String): SystemMail = {
    val mapBean = MapBean.toMutableMap(mailJsonString)
    MapBean.map2Bean(mapBean, classOf[SystemMail])
  }

  def systemMail2Json(systemMail: SystemMail): String = {
    systemMail.toString
  }

  def main(args: Array[String]): Unit = {
    val date = LocalDateTime.of(2021, 1, 1, 0, 14, 0)
    println(date)


  }
}
