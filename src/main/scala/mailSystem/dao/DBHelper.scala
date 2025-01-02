package mailSystem.dao

object DBHelper {
  private final val HOST = "localhost"
  private final val PORT = 3306
  private final val DATABASE = "gql_mailsystem"
  private final val USER = "root"
  private final val PASSWORD = "root"
  private final val TIMEZONE = "UTC"
  private final val SSL = "false"
  private final val URL = s"jdbc:mysql://$HOST:$PORT/$DATABASE?useSSL=$SSL&serverTimezone=$TIMEZONE"

  private def connection: java.sql.Connection = {
    java.sql.DriverManager.getConnection(URL, USER, PASSWORD)
  }

  def closeConnection(connection: java.sql.Connection): Unit = {
    connection.close()
  }

  def executeQuery(sql: String, para: Array[Any]): java.sql.ResultSet = {
    try {
      val connection = DBHelper.connection
      val ps = connection.prepareStatement(sql)
      for (i <- para.indices) {
        ps.setObject(i + 1, para(i))
      }
      ps.executeQuery()
    } catch {
      case e: Exception => throw new Exception(e)
    }
  }

  def executeUpdate(sql: String, para: Array[Any]): Unit = {
    try {
      val connection = DBHelper.connection
      val ps = connection.prepareStatement(sql)
      for (i <- para.indices) {
        ps.setObject(i + 1, para(i))
      }
      ps.executeUpdate()
    } catch {
      case e: Exception => throw new Exception(e)
    }
  }

  def add(sql: String, para: Any*): Unit = {
    try {
      val connection = DBHelper.connection
      val ps = connection.prepareStatement(sql)
      for (i <- para.indices) {
        println(para(i))
        ps.setObject(i + 1, para(i))
      }
      ps.executeUpdate()
    } catch {
      case e: Exception => {
        throw new Exception("Failed to add")
      }
    } finally {
      DBHelper.closeConnection(DBHelper.connection)
    }
  }

  def update(sql: String, para: Any*): Unit = {
    try {
      val connection = DBHelper.connection
      val ps = connection.prepareStatement(sql)
      for (i <- para.indices) {
        ps.setObject(i + 1, para(i))
      }
      ps.executeUpdate()
    } catch {
      case e: Exception => throw new Exception("Failed to update")
    } finally {
      DBHelper.closeConnection(DBHelper.connection)
    }
  }

  def query(sql: String, para: Any*): java.sql.ResultSet = {
    try {
      val connection = DBHelper.connection
      val ps = connection.prepareStatement(sql)
      for (i <- para.indices) {
        ps.setObject(i + 1, para(i))
      }
      ps.executeQuery()
    } catch {
      case e: Exception => throw new Exception("Failed to query")
    }
  }

  def delete(sql: String, para: Any*): Unit = {
    try {
      val connection = DBHelper.connection
      val ps = connection.prepareStatement(sql)
      for (i <- para.indices) {
        ps.setObject(i + 1, para(i))
      }
      ps.executeUpdate()
    } catch {
      case e: Exception => throw new Exception("Failed to delete")
    } finally {
      DBHelper.closeConnection(DBHelper.connection)
    }
  }
}
