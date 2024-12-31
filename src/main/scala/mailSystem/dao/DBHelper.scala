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

  def connection: java.sql.Connection = {
    java.sql.DriverManager.getConnection(URL, USER, PASSWORD)
  }

  def closeConnection(connection: java.sql.Connection): Unit = {
    connection.close()
  }

}
