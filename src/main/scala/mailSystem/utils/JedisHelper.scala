package mailSystem.utils

import redis.clients.jedis.{Jedis, JedisPool, JedisPoolConfig}

import java.util.ResourceBundle
import scala.concurrent.duration.{Duration, MILLISECONDS}

object JedisHelper {
  private val config = new JedisPoolConfig()
  private val bundle = ResourceBundle.getBundle("jedis")

  private val HOST = bundle.getString("host")
  private val PORT = bundle.getString("port").toInt
  private val MAX_TOTAL = bundle.getString("max_total").toInt
  private val MAX_WAIT_MILLIS = bundle.getString("max_wait_millis").toInt

  println("JedisHelper:")
  println(HOST, PORT, MAX_TOTAL, MAX_WAIT_MILLIS)
  config.setMaxTotal(MAX_TOTAL)
  config.setMaxWaitMillis(MAX_WAIT_MILLIS)

  private val logger = Log4jUtils.getLogger(this.getClass)
  private val jedisPoll: JedisPool = new JedisPool(config, HOST, PORT)

  def getJedis: Jedis = {
    try {
      jedisPoll.getResource
    } catch {
      case e: Exception =>
        logger.error("获取Jedis连接失败", e)
        null
    }
  }

  def closeJedis(jedis: Jedis): Unit = {
    if (jedis != null)
      try {
        jedis.close()
        jedis.close()
      } catch {
        case e: Exception =>
          println(e)
          logger.error("关闭Jedis连接失败", e)
      }
  }

  def closePool(): Unit = {
    if (jedisPoll != null)
      try {
        jedisPoll.close()
      } catch {
        case e: Exception =>
          logger.error("关闭JedisPool失败", e)
      }
  }

  def main(args: Array[String]): Unit = {
    val jedis = JedisHelper.getJedis
    jedis.set("test", "test")
    println(jedis.get("test"))
    jedis.sadd("testSet", "test1", "test2", "test3")
    val testSet = jedis.smembers("testSet")
    testSet.forEach(println)
    jedis.lpush("testList", "test1", "test2", "test3")
    val testList = jedis.lrange("testList", 0, -1)
    println(testList)
    testList.forEach(println)
    JedisHelper.closeJedis(jedis)
  }
}
