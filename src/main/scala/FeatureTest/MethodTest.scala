package FeatureTest

object MethodTest extends App {
  val list = List(1, 2, 3, 4, 5, 6)
  val parts = list.grouped(2).toList
  println(parts)
  val a = Vector(1, 2, 3, 4, 5, 6)
}
