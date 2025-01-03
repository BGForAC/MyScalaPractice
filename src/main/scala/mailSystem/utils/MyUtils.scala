package mailSystem.utils

import java.time.LocalDateTime
import java.util.Properties

object MyUtils {
  private def nextMoment(year: Int)(month: Int)(day: Int)(hour: Int)(minute: Int)(seconds: Int)(now: LocalDateTime): LocalDateTime = {
    now.plusYears(year).plusMonths(month).plusDays(day).plusHours(hour).plusMinutes(minute).plusSeconds(seconds)
  }

//  val nextYear: LocalDateTime => LocalDateTime = nextMoment(1)(0)(0)(0)(0)(0)
//  val nextMonth: LocalDateTime => LocalDateTime = nextMoment(0)(1)(0)(0)(0)(0)
//  val nextDay: LocalDateTime => LocalDateTime = nextMoment(0)(0)(1)(0)(0)(0)
//  val nextHour: LocalDateTime => LocalDateTime = nextMoment(0)(0)(0)(1)(0)(0)
//  val nextMinute: LocalDateTime => LocalDateTime = nextMoment(0)(0)(0)(0)(1)(0)
//  val nextSecond: LocalDateTime => LocalDateTime = nextMoment(0)(0)(0)(0)(0)(1)
//  val momentNext: Int => Int => Int => Int => Int => Int => LocalDateTime => LocalDateTime = nextMoment
//  val yearNext: Int => Int => Int => Int => Int => LocalDateTime => LocalDateTime = nextMoment(1)
//  val monthNext: Int => Int => Int => Int => LocalDateTime => LocalDateTime = nextMoment(0)(1)
//  val dayNext: Int => Int => Int => LocalDateTime => LocalDateTime = nextMoment(0)(0)(1)
//  val hourNext: Int => Int => LocalDateTime => LocalDateTime = nextMoment(0)(0)(0)(1)
//  val minuteNext: Int => LocalDateTime => LocalDateTime = nextMoment(0)(0)(0)(0)(1)

  val generateAlphaNumericRandomLength = generateStringRandomLength(RandomGenerator.generateRandomAlphaNumeric) _
  val generateAlphaRandomLength = generateStringRandomLength(RandomGenerator.generateRandomAlpha) _
  val generateLowerAlphaRandomLength = generateStringRandomLength(RandomGenerator.generateRandomLowerAlpha) _
  val generateUpperAlphaRandomLength = generateStringRandomLength(RandomGenerator.generateRandomUpperAlpha) _
  val generateNumericRandomLength = generateStringRandomLength(RandomGenerator.generateRandomNumeric) _
  val generateAlphaNumericFixedLength = generateStringRandomLength(RandomGenerator.generateRandomAlphaNumeric)(0) _
  val generateAlphaFixedLength = generateStringRandomLength(RandomGenerator.generateRandomAlpha)(0) _
  val generateLowerAlphaFixedLength = generateStringRandomLength(RandomGenerator.generateRandomLowerAlpha)(0) _
  val generateUpperAlphaFixedLength = generateStringRandomLength(RandomGenerator.generateRandomUpperAlpha)(0) _
  val generateNumericFixedLength = generateStringRandomLength(RandomGenerator.generateRandomNumeric)(0) _

  def generateStringRandomLength(rules: Int => Iterable[Char])(offset: Int)(length: Int): String = {
    val random = new scala.util.Random
    val fixedLength = (if (offset == 0) 0 else random.nextInt(offset)) + length
    rules(fixedLength).mkString
  }

  private object RandomGenerator {
    private val random = new scala.util.Random

    def generateRandomAlphaNumeric(length: Int): String = random.alphanumeric.take(length).mkString

    private def randomAlpha(): Char = {
      val x = random.nextInt(52)
      val base = if (x < 26) 'A' else 'a'
      (base + x % 26).toChar
    }

    def generateRandomAlpha(length: Int): String = (for (_ <- 1 to length) yield randomAlpha()).mkString

    private def randomLowerAlpha(): Char = (random.nextInt(26) + 'a').toChar

    def generateRandomLowerAlpha(length: Int): String = (for (_ <- 1 to length) yield randomLowerAlpha()).mkString

    private def randomUpperAlpha(): Char = (random.nextInt(26) + 'A').toChar

    def generateRandomUpperAlpha(length: Int): String = (for (_ <- 1 to length) yield randomUpperAlpha()).mkString

    private def randomNumeric(): Char = (random.nextInt(10) + '0').toChar

    def generateRandomNumeric(length: Int): String = (for (_ <- 1 to length) yield randomNumeric()).mkString
  }

  def readConfig(config: Properties, filename: String): Properties = {
    val source = scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(filename))
    try {
      for (line <- source.getLines) {
        if (line.exists(_ == '=')) {
          val Array(key, value) = line.split("=", 2)
          key match {
            case _ if key.startsWith("//") => // 注释，忽略
            case _ if key.trim.isEmpty => // 空键，忽略
            case _ => config.setProperty(key.trim, value.trim)
          }
        }
      }
    } finally {
      source.close()
    }
    config
  }

  def main(args: Array[String]): Unit = {
    val iterations = 10
    val generateRandomAlphaNumeric = generateStringRandomLength(RandomGenerator.generateRandomAlphaNumeric) _
    for (_ <- 1 to iterations) {
      println(generateRandomAlphaNumeric(0)(10))
      println(generateRandomAlphaNumeric(10)(10))
      println(generateRandomAlphaNumeric(3)(8))
    }
  }
}
