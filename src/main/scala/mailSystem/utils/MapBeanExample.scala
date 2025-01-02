package mailSystem.utils

object MapBeanExample{

  def main(args: Array[String]): Unit = {
    stringMapTest()
    recMapTest()
  }

  def recMapTest() = {
    val strMap = Map("mail_read" -> Map("mail_id" -> "1", "mail_id2" -> "2"), "mail_receive" -> Map("mail_id" -> "3", "mail_id2" -> "4"))
    val str = GsonUtils.map2Json(strMap)
    println(str)
    val strMap2 = MapBean.toMutableMap(str)
    println(strMap2)
  }

  def stringMapTest() = {
    val strMap = Map("mail_read" -> "1,2,3", "mail_receive" -> "4,5,6")
    val str = GsonUtils.map2Json(strMap)
    println(str)
    val strMap2 = MapBean.toMutableMap(str)
    println(strMap2)
  }
}
