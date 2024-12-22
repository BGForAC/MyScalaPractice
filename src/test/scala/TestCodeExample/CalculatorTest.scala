package TestCodeExample

import TestCodeExample.Calculator
import org.scalatest.funsuite.AnyFunSuite

class CalculatorTest extends AnyFunSuite {

  test("testAdd") {
    val calculator = new Calculator()
    val result = calculator.add(2, 3)
    assert(result === 6, "加法有误")
  }

}