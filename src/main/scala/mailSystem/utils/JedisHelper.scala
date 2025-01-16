package mailSystem.utils

import redis.clients.jedis.params.SetParams
import redis.clients.jedis.{Jedis, JedisPool, JedisPoolConfig}

import java.util
import java.util.ResourceBundle

/**
 * 工具类，用于Redis操作
 */
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

  private def itemId2Key(itemId: Long): String = s"item:$itemId"

  private def playerId2Key(playerId: Long): String = s"player:$playerId"

  private def systemMailId2Key(systemMailId: Long): String = s"system_mail:$systemMailId"

  def distributionKeyForAttachmentCollect(playerId: Long, mailId: Long): String = s"distribution:attachment:collect:$playerId:$mailId"

  def distributionKeyForStatusChange: String = "distribution:status:change"

  private def keyForSystemMail = "system_mail"

  private def keyForMailsRead = "mails_read"

  private def keyForMailsCollect = "mails_collect"

  private def keyForItems = "items"

  def key2PlayerId(key: String): Option[Long] = {
    key match {
      case key if key.startsWith("player:") => Some(key.split(":")(1).toLong)
      case _ => None
    }
  }

  private def del(jedis: Jedis, key: String): Unit = {
    jedis.del(key)
  }

  private def exists(jedis: Jedis, key: String): Boolean = {
    jedis.exists(key)
  }

  private def set(jedis: Jedis, key: String, value: String): Unit = {
    jedis.set(key, value)
  }

  private def hexists(jedis: Jedis, key: String, field: String): Boolean = {
    jedis.hexists(key, field)
  }

  private def hget(jedis: Jedis, key: String, field: String): String = {
    jedis.hget(key, field)
  }

  private def hset(jedis: Jedis, key: String, field: String, value: String): Unit = {
    jedis.hset(key, field, value)
  }

  private def hdel(jedis: Jedis, key: String, field: String): Unit = {
    jedis.hdel(key, field)
  }

  def playerDisconnect(jedis: Jedis, playerId: Long): Unit = {
    del(jedis, playerId2Key(playerId))
  }

  def playerReadStatusLoaded(jedis: Jedis, playerId: Long): Boolean = {
    hexists(jedis, playerId2Key(playerId), keyForMailsRead)
  }

  def playerCollectStatusLoaded(jedis: Jedis, playerId: Long): Boolean = {
    hexists(jedis, playerId2Key(playerId), keyForMailsCollect)
  }

  def getPlayerReadStatus(jedis: Jedis, playerId: Long): String = {
    hget(jedis, playerId2Key(playerId), keyForMailsRead)
  }

  def getPlayerCollectStatus(jedis: Jedis, playerId: Long): String = {
    hget(jedis, playerId2Key(playerId), keyForMailsCollect)
  }

  def updateReadStatus(jedis: Jedis, playerId: Long, content: String): Unit = {
    hset(jedis, playerId2Key(playerId), keyForMailsRead, content)
  }

  def updateCollectStatus(jedis: Jedis, playerId: Long, content: String): Unit = {
    hset(jedis, playerId2Key(playerId), keyForMailsCollect, content)
  }

  def deleteReadStatusCache(jedis: Jedis, playerId: Long): Unit = {
    hdel(jedis, playerId2Key(playerId), keyForMailsRead)
  }

  def deleteCollectStatusCache(jedis: Jedis, playerId: Long): Unit = {
    hdel(jedis, playerId2Key(playerId), keyForMailsCollect)
  }

  def getSystemMails(jedis: Jedis): util.Map[String, String] = {
    jedis.hgetAll(keyForSystemMail)
  }

  def addSystemMail(jedis: Jedis, systemMailId: Long, content: String): Unit = {
    hset(jedis, keyForSystemMail, systemMailId2Key(systemMailId), content)
  }

  def deleteSystemMailsCache(jedis: Jedis): Unit = {
    del(jedis, keyForSystemMail)
  }

  def getItem(jedis: Jedis, itemId: Long): String = {
    hget(jedis, keyForItems, itemId2Key(itemId))
  }

  def addItem(jedis: Jedis, itemId: Long, content: String): Unit = {
    hset(jedis, keyForItems, itemId2Key(itemId), content)
  }

  def cachedPlayersIds(jedis: Jedis): Array[Long] = {
    jedis.keys("*").toArray.flatMap(key => key2PlayerId(key.asInstanceOf[String]))
  }

  def deletePlayerCache(jedis: Jedis, playerId: Long): Unit = {
    del(jedis, playerId2Key(playerId))
  }

  private def flushRedis(): Unit = {
    val jedis = this.getJedis
    try {
      jedis.flushAll()
      println("Redis已清空")
    } catch {
      case e: Exception =>
        logger.error("清空Redis失败", e)
//        throw e
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
      } catch {
        case e: Exception =>
          logger.error("关闭Jedis连接失败", e)
          throw e
      }
    else {
      logger.error("尝试关闭空Jedis连接")
    }
  }

  private def closePool(): Unit = {
    if (jedisPoll != null)
      try {
        jedisPoll.close()
      } catch {
        case e: Exception =>
          logger.error("关闭JedisPool失败", e)
          throw e
      }
    else {
      logger.warn("尝试关闭空的JedisPool")
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
        logger.error(s"获取分布式锁失败, key: $lockKey, value: $lockValue")
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