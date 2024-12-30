package FeatureTest

object ArrayTest extends App {
  val a = Array(1, 2, 3, 5, 4)

  println(Array(1, 2, 3) ++ Array(4, 5, 6))

  a.sorted

  println {
    a.apply(2)
  }

  Predef println a.apply(2)

  a.foreach(println)
}
