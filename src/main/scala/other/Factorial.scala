package other

import scala.annotation.tailrec

object Factorial extends App {
  println(factorial(50))
  println(combination(5, 2))
  println(combination(10, 5))

  private def factorial(a: BigInt): BigInt = {
    @tailrec
    def loop(a: BigInt, i: BigInt): BigInt = {
      if (a == 0) i
      else loop(a - 1, i * a)
    }

    loop(a, 1)
  }

  private def combination(a: Int, b: Int): BigInt = {
    assert(a >= b, "组合数的参数不规范")
    factorial(a) / (factorial(b) * factorial(a - b))
  }
}

