package Solutions

class Solution4 {
  def minSetSize(arr: Array[Int]): Int = arr
    .groupBy(x => x)
    .map(_._2.length)
    .toSeq
    .sortWith(_ > _)
    .scanLeft(0)(_ + _)
    .indexWhere(_ >= arr.length / 2)
  val a = List(1,2,3).head
  scala.collection.mutable.ListBuffer

}
