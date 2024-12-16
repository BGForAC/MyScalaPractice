package gql

object ListTest extends App {
  val a = List(1, 2, 3, 4, 5)

  var cList = List("123", "243", "456")

  def reverseLeft[T](sx: List[T]): List[T] = {
    (List[T]() foldLeft sx) { (sx, sy) => sy :: sx }
  }

  def reverseRight[T](sx: List[T]): List[T] = {
    (sx foldRight List[T]()) { (sx, sy) => sy ::: List(sx) }
  }

  reverseLeft(a).foreach(println)

  a.map(_ + 10).foreach(println(_))

  cList.flatMap(_.toList).foreach(println)

  reverseRight(a).foreach(println)
}
