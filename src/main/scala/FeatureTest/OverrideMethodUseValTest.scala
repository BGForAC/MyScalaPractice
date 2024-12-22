package FeatureTest

class OverrideMethodUseValTest {
  class A {
    val foo = 1
    def bar(x: Int): Int = x
    def bar = 2
  }

  class B extends A {
    override val foo = 3
    override val bar = 4
  }
}

object OverrideMethodUseValTest extends App {
  val obj = new OverrideMethodUseValTest
  val b = new obj.B
  val a = new obj.A
  println(b.foo)
  println(b.bar)
  println(a.foo)
  println(a.bar)
  println(b.bar(5))
}
