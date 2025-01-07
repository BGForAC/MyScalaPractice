package mailSystem.utils

import mailSystem.entity.SystemMail

object MapBeanUtils {

  def map2Json(map: Map[String, Any]): String = {
    var mapBean: MapBean = MapBean.empty
    map.foreach{mapBean += _}
    mapBean.toJsonString
  }

  def json2SystemMail(mail: String): SystemMail = {
    val mapBean = MapBean.toMutableMap(mail)
    MapBean.map2Bean(mapBean, classOf[SystemMail])
  }

  def systemMail2Json(systemMail: SystemMail): String = {
    systemMail.toString
  }

  def main(args: Array[String]): Unit = {
    val json = "{mail_id=1131, content=content, title=title, attachment=awd, filter=}, publicTime=null, deadline=null, createTime=null, updateTime=null, read=false, collect=false}"
    val mail2 = json2SystemMail(json)
    println(mail2)

  }
}
