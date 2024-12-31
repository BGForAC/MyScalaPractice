package mailSystem.tools

import org.apache.log4j.LogManager

object Log4jUtils {
  def getLogger(clazz: Class[_]): org.apache.log4j.Logger = {
    LogManager.getLogger(clazz)
  }

}
