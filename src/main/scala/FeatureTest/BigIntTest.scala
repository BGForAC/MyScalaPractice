package FeatureTest

object BigIntTest extends App{
  val a: BigInt = BigInt(10)
  println(a.toString(2))
  a.setBit(0)
  println(a.setBit(2))
  println(a >> 1)
  println(BigInt(1) << 0 & a)
}
