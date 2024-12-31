package mailSystem.tools

object GsonExample{

  def main(args: Array[String]): Unit = {
    strMapTransformTest()
  }

  def recMapTransformTest() = {
    val strMap = Map("mail_read" -> Map("mail_id" -> "1", "mail_id2" -> "2"), "mail_receive" -> Map("mail_id" -> "3", "mail_id2" -> "4"))
    val str = GsonUtils.recMapToJson(strMap)
    println(str)
    val strMap2 = GsonUtils.recMapFromJson(str)
    println(strMap2)
  }

  def strMapTransformTest() = {
    val strMap = Map("mail_read" -> "1,2,3", "mail_receive" -> "4,5,6")
    val str = GsonUtils.strMapToJson(strMap)
    println(str)
    val strMap2 = GsonUtils.strMapFromJson(str)
    println(strMap2)
  }
}
