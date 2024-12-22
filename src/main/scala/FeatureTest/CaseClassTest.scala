package FeatureTest

import scala.collection.mutable

object CaseClassTest extends App {
  /**
   * 样例类是否可以有伴生对象？
   */
  abstract class Expr
  case class Val(value: String) extends Expr
  case class BinOp(arg1: String, arg2: Expr, arg3: Expr) extends Expr

  object BinOp {
    def apply(arg1: String, arg2: Expr, arg3: String): Expr = {Val("12")}
  }

  val expr = BinOp("+", Val("12"), Val("12"))

  print(expr)

  val a = Test("123")
  a.map += (1 -> 2)
  print(a.map(1))

  class Test {
    val map = mutable.HashMap[Int, Int]()
  }

  object Test {
    def apply(str: String) = {
      new Test
    }
  }
}
