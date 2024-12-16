package gql

import scala.annotation.tailrec
import scala.language.implicitConversions

class Rational (n: Int, d: Int){

  val numer: Int = n

  val denom: Int = d

  val g: Int = gcd(n.abs, d.abs)

  def +(that: Rational): Rational = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def -(that: Rational): Rational = {
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )
  }

  def *(that: Rational): Rational = {
    new Rational(
      numer * that.numer,
      denom * that.denom
    )
  }

  def /(that: Rational): Rational = {
    new Rational(
      numer * that.denom,
      denom * that.numer
    )
  }

  override def toString: String = {
    val g = gcd(n.abs, d.abs)
    (n / g) + "/" + (d / g)
  }

  @tailrec
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b);
  }
}

object Rational {
  def main(args: Array[String]): Unit = {
    implicit def intToRational(x: Int): Rational = new Rational(x, 1)
    val x = new Rational(1, 2)
    val y = new Rational(2, 3)
    val z = new Rational(3, 4)
    val t = x + 10
    val m = 10 + x
    println(4+x + y * z+5)
  }
}
