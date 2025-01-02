package mailSystem.utils

object SnowflakeIdGeneratorExample {
  def main(args: Array[String]): Unit = {
    val idGenerator = new SnowflakeIdGenerator(0, 0)
    for (_ <- 1 to 10) {
      println(idGenerator.nextId())
    }
  }
}
