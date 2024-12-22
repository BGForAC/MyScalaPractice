package FeatureTest

import scala.collection.mutable

object ObjectEqualityTest extends App {
  TestGroup1()
  println("--------------------")
  TestGroup2()

  def TestGroup1(): Unit = {
    val a1, b1 = new Pair(1, 2)
    val set = mutable.HashSet(a1)
    val b1a: Any = b1
    println(set.contains(b1))
    println(a1 == b1)
    println(a1 equals b1)
    println(a1 equals b1a)
  }

  def TestGroup2(): Unit = {
    val a1, b1 = new BetterPair(1, 2)
    val set = mutable.HashSet(a1)
    val b1a: Any = b1
    println(set.contains(b1))
    println(a1 == b1)
    println(a1 equals b1)
    println(a1 equals b1a)
  }
}

class Pair(val x: Int, val y: Int) {
  def equals(obj: Pair): Boolean = {
    obj.x == this.x && obj.y == this.y
  }
}

class BetterPair(val x: Int, val y: Int) {
  override def equals(obj: Any): Boolean = obj match {
    case obj: BetterPair => obj.x == this.x && obj.y == this.y
    case _ => false
  }
}
