package mailSystem.tools

import scala.io.Source

object ResourceLoaderTest extends App {
  val resourcePath = "log4j.properties" // 替换为你要检验的文件名
  val resourceStream = getClass.getClassLoader.getResourceAsStream(resourcePath)

  if (resourceStream != null) {
    val source = Source.fromInputStream(resourceStream)
    val content = source.getLines().mkString("\n")
    source.close()
    println(s"File '$resourcePath' loaded successfully. Content:\n$content")
  } else {
    println(s"File '$resourcePath' not found in resources.")
  }
}
