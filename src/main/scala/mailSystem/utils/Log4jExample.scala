package mailSystem.utils

object Log4jExample extends App {
  val logger = Log4jUtils.getLogger(getClass)

  logger.debug("This is a debug message")
  logger.info("This is an info message")
  logger.warn("This is a warning message")
  logger.error("This is an error message")
}
