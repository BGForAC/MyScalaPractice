package FeatureTest

object StringTest extends App {
  println("123124".slice(4, 5))
  println("123123".slice(5, 5))
  println("123".slice(0, 10))
  val a = "123"
  println("123" + a)

  def matchString(str: String) = {
    str match {
//      case "1" + (e: String) => println(e) // 编译不通过
      case _ => println("default")
    }
  }

  matchString("123")
}
