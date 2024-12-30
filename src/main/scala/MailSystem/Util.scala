package MailSystem

object Util {

  private def generateRandomName(length: Int, offset: Int): String = {
    val random = new scala.util.Random
    val fixedLength = (if (offset == 0) 0 else random.nextInt(offset)) + length
    random.alphanumeric.take(fixedLength).mkString
  }

  def main(args: Array[String]): Unit = {
    val iterations = 10
    for (i <- 1 to iterations)
    println(generateRandomName(3, 0))
  }

}
