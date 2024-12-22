package FeatureTest

import scala.collection.mutable.ArrayBuffer

object SwapTest extends App{
  val a = Array.ofDim[Int](1,2,3,4,5)
  a.foreach(println)
  val b = a.flatten.flatten.flatten
  b.foreach(println)
  b.flatten.foreach(println)
  print(Array(1, 2, 3, 4).mkString("Array(", ", ", ")"))
}
