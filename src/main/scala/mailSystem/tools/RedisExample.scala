package mailSystem.tools

import redis.clients.jedis.Jedis

object RedisExample extends App {
  // Connect to Redis server
  val jedis = new Jedis("localhost", 6379)

  // Set a key-value pair
  jedis.set("scala-key", "Hello, Redis!")

  // Get the value of a key
  val value = jedis.get("scala-key")
  println(s"Value for 'scala-key': $value")

  // Increment a key
  jedis.set("counter", "1")
  jedis.incr("counter")
  val counterValue = jedis.get("counter")
  println(s"Value for 'counter': $counterValue")

  // Close the connection
  jedis.close()
}
