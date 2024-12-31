package mailSystem.service

import mailSystem.dao.DBHelper

object UserService {

  def userInfo(userId: BigInt): String = {
    val sql = "select (mail_read, mail_receive) from users where id = ?"
    try {
      val connection = DBHelper.connection
      val ps = connection.prepareStatement(sql)
      ps.setBigDecimal(1, new java.math.BigDecimal(userId.bigInteger))
      val rs = ps.executeQuery()
      "{\n" + rs.getString("mail_read") + rs.getString("mail_receive") + "\n}"
    } catch {
      case e: Exception => throw new Exception("Failed to get user info")
    } finally {
      DBHelper.closeConnection(DBHelper.connection)
    }
  }

  def addUser(name: String, mail_read: String, mail_receive: String): Unit = {
    val sql = "insert into users (name, mail_read, mail_receive) values (?, ?, ?)"
    try {
      val connection = DBHelper.connection
      val ps = connection.prepareStatement(sql)
      ps.setString(1, name)
      ps.setString(2, mail_read)
      ps.setString(3, mail_receive)
      ps.executeUpdate()
    } catch {
      case e: Exception => throw new Exception("Failed to add user")
    } finally {
      DBHelper.closeConnection(DBHelper.connection)
    }
  }

}
