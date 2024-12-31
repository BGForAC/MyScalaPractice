package mailSystem.service

import mailSystem.tools.{GsonUtils, JedisUtils}

object MailService {

  def loadUserInfo(userId: BigInt): Unit = {
    val jedis = JedisUtils.jedis
    if (jedis.ping() != "PONG") throw new Exception("Failed to connect to redis")

    try {
      val userInfo: String = UserService.userInfo(userId)
      jedis.set(userId.toString(), userInfo)
    }catch {
      case e: Exception => println("Failed to load user info")
    }finally {
      JedisUtils.closeJedis(jedis)
    }
  }

  def readMail(mailId: BigInt, userId: BigInt): Unit = {
    val jedis = JedisUtils.jedis
    val userInfo = GsonUtils.strMapFromJson(jedis.get("personId"))
    userInfo.get("mail_read") match {
      case Some(mail_read) => {
        val mail_read_list = mail_read.split(",")
        if (!mail_read_list.contains(mailId.toString)) {
          mail_read_list :+ mailId.toString
          userInfo.updated("mail_read", mail_read_list.mkString(","))
          jedis.set(userId.toString(), GsonUtils.strMapToJson(userInfo))
        }
      }
      case None => {
        userInfo.updated("mail_read", mailId.toString)
        jedis.set(userId.toString(), GsonUtils.strMapToJson(userInfo))
      }
    }

  }

  def sendMail(): Unit = {
  }

}
