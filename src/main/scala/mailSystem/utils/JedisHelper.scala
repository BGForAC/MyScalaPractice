package mailSystem.utils

import redis.clients.jedis.params.SetParams
import redis.clients.jedis.{Jedis, JedisPool, JedisPoolConfig}

import java.util.ResourceBundle

object JedisHelper {
  private val config = new JedisPoolConfig()
  private val bundle = ResourceBundle.getBundle("jedis")

  private val HOST = bundle.getString("host")
  private val PORT = bundle.getString("port").toInt
  private val MAX_TOTAL = bundle.getString("max_total").toInt
  private val MAX_WAIT_MILLIS = bundle.getString("max_wait_millis").toInt

  config.setMaxTotal(MAX_TOTAL)
  config.setMaxWaitMillis(MAX_WAIT_MILLIS)

  private val logger = Log4jUtils.getLogger(this.getClass)
  private val jedisPoll: JedisPool = new JedisPool(config, HOST, PORT)

  private def flushRedis(): Unit = {
    val jedis = this.getJedis
    try {
      jedis.flushAll()
    } catch {
      case e: Exception =>
        logger.error("清空Redis失败", e)
    } finally {
      closeJedis(jedis)
    }
  }

  flushRedis()

  private def getJedis: Jedis = {
    try {
      jedisPoll.getResource
    } catch {
      case e: Exception =>
        logger.error("获取Jedis连接失败", e)
        throw e
    }
  }

  private def closeJedis(jedis: Jedis): Unit = {
    if (jedis != null)
      try {
        jedis.close()
//        jedis.close() // 重复关闭不会报错
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

  def execute[T](call: Jedis => T): T = {
    val jedis = this.getJedis
    try {
      call(jedis)
    } catch {
      case e: Exception =>
        logger.error("Jedis执行失败", e)
        throw e
    }finally {
      closeJedis(jedis)
    }
  }

  def executeWithDistributionLock[T](lockKey: String, lockValue: String, expireTime: Int)(call: Jedis => T): T = {
    execute { jedis =>
      val set = jedis.set(lockKey, lockValue, new SetParams().nx().ex(expireTime))
      if (set == "OK") {
        try {
          call(jedis)
        } finally {
          jedis.del(lockKey)
        }
      } else {
        throw new Exception(s"获取分布式锁失败, key: $lockKey, value: $lockValue")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val jedis = this.getJedis
    jedis.set("test", "test")
    jedis.sadd("testSet", "test1", "test2", "test3")
    val testSet = jedis.smembers("testSet")
    jedis.lpush("testList", "test1", "test2", "test3")
    val testList = jedis.lrange("testList", 0, -1)
    testList.forEach(println)
    println(testList.size())
    closeJedis(jedis)
  }
}