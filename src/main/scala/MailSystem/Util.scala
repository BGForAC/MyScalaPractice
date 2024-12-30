package MailSystem

object Util {

  private def generateRandomName(length: Int)(offset: Int)(rules: Int => Iterable[Char]): String = {
    val random = new scala.util.Random
    val fixedLength = (if (offset == 0) 0 else random.nextInt(offset)) + length
    rules(fixedLength).mkString
  }

  def main(args: Array[String]): Unit = {
    val iterations = 10
    for (i <- 1 to iterations)
    println(generateRandomName(3)(0)(scala.util.Random.alphanumeric.take _))
  }

}
