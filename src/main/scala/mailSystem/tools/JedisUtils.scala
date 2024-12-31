package mailSystem.tools

import redis.clients.jedis.{Jedis, JedisPool}

object JedisUtils {
  private final val HOST = "localhost"
  private final val PORT = 6379

  private val logger = Log4jUtils.getLogger(this.getClass)
  private val jedisPoll: JedisPool = new JedisPool(HOST, PORT)

  /**
   * 获取Jedis连接，失败返回null
   * @return
   */
  def jedis: Jedis = {
    jedisPoll.getResource
  }

  def closeJedis(jedis: Jedis): Unit = {
    jedis.close()
  }
}
