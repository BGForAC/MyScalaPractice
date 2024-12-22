package other

object ListMatcher extends App {
  print(matcher(List(1)))

  def matcher(list: List[Int]): Int = {
    list match {
      case Nil => -1
      case _ :: last :: Nil => last
      case last :: Nil => last
      case first :: _ :: Nil => first
      case _ => -2
    }
  }
}

