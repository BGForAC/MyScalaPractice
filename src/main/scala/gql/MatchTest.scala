package gql

object MatchTest {
  def generalSize(x:Any):Any={x match{case a:Iterable[_]=>a.size;case _ => "not a collection"}}

  def main(args: Array[String]): Unit = {
    System.out.println(generalSize(List(1,2,3)))
    System.out.println(generalSize("hello"))
    System.out.println(generalSize(Seq(1,2,3)))
  }
}
