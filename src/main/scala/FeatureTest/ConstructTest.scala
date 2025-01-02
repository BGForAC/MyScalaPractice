package FeatureTest

class ConstructTest {

  case class test1(a: Int, b: Int = 2)

  test1(2)

  case class test2(a: Int = 1, b: Int)

//  test2(2)
}

class test3(var a: Int, var b: Int, var c: Int) {
  println(s"constructor: $a, $b, $c")

  def this() = {
    this(1, 2, 3)
    println(s"f1: $a, $b, $c")
  }

  def this(a: Int) = {
    this()
    this.a = a
    println(s"f2: $a, $b, $c")
  }

  def this(a: Int, b: Int) = {
    this(a)
    this.b = b
    println(s"f3: $a, $b, $c")
  }

  override def toString: String = {
    s"test3($a, $b, $c)"
  }

}

object test3 {
  def main(args: Array[String]): Unit = {
    val t4 = new test3(10, 11)
    println(t4)
  }
}