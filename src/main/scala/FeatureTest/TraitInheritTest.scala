package FeatureTest

object TraitInheritTest extends App {
  trait A {
    def message = "A"
  }

  trait B extends A {
    override def message: String = "B" + super.message
  }

  trait C extends A {
    override def message: String = "C" + super.message
  }

  class D extends A{
    override def message: String = "D" + super.message
  }

  val b = new D with B
  println(b.message)

  val c = new D with C
  println(c.message)

  val d = new D
  println(d.message) // 输出: "DCBA"
}
