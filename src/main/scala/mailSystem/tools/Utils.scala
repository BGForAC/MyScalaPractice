package mailSystem.tools

object Utils {
  private def generateRandomString(rules: Int => Iterable[Char])(offset: Int)(length: Int): String = {
    val random = new scala.util.Random
    val fixedLength = (if (offset == 0) 0 else random.nextInt(offset)) + length
    rules(fixedLength).mkString
  }

  object RandomGenerator {
    private val random = new scala.util.Random
    def generateRandomAlphaNumeric(length: Int): String = {
      random.alphanumeric.take(length).mkString
    }

    private def randomAlpha(): Char = {
      val x = random.nextInt(52)
      val base = if (x < 26) 'A' else 'a'
      (base + x % 26).toChar
    }

    def generateRandomAlpha(length: Int): String = {
      (for (_ <- 1 to length) yield randomAlpha()).mkString
    }

    private def randomLowerAlpha(): Char = {
      (random.nextInt(26) + 'a').toChar
    }

    def generateRandomLowerAlpha(length: Int): String = {
      (for (_ <- 1 to length) yield randomLowerAlpha()).mkString
    }

    private def randomUpperAlpha(): Char = {
      (random.nextInt(26) + 'A').toChar
    }

    def generateRandomUpperAlpha(length: Int): String = {
      (for (_ <- 1 to length) yield randomUpperAlpha()).mkString
    }

    private def randomNumeric(): Char = {
      (random.nextInt(10) + '0').toChar
    }

    def generateRandomNumeric(length: Int): String = {
      (for (_ <- 1 to length) yield randomNumeric()).mkString
    }
  }

  def main(args: Array[String]): Unit = {
    val iterations = 10
    val generateRandomAlphaNumeric = generateRandomString(RandomGenerator.generateRandomAlphaNumeric) _
    for (_ <- 1 to iterations) {
      println(generateRandomAlphaNumeric(0)(10))
      println(generateRandomAlphaNumeric(10)(10))
      println(generateRandomAlphaNumeric(3)(8))
    }
  }
}
