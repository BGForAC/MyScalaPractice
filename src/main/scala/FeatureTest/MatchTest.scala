package FeatureTest

object MatchTest {
  def generalSize(x:Any):Any={x match{case a:Iterable[_]=>a.size;case _ => "not a collection"}}

  def f1: PartialFunction[Any, Int] = { case a: Iterable[_] => a.size }
  def f2: PartialFunction[Any, String] = { case _ => "not a collection" }

  def matchFunctionType(f: PartialFunction[Any, _]): String = f match {
    case _: PartialFunction[_, Int] => "PartialFunction[Any, Int]"
    case _: PartialFunction[_, String] => "PartialFunction[Any, String]"
    case _ => "Unknown"
  }


  def main(args: Array[String]): Unit = {
    println(matchFunctionType(f1))
    println(matchFunctionType(f2))
  }
}
