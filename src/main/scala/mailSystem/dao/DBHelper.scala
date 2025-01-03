package mailSystem.dao

import com.alibaba.druid.pool.DruidDataSourceFactory
import mailSystem.utils.{Log4jUtils, MyUtils}

import java.sql.Connection
import java.util.Properties
import javax.sql.DataSource

object DBHelper {
  private var dataSource: DataSource = _

  private val druidProps = MyUtils.readConfig(new Properties(), "druid.properties")

  private def initDataSource(): Unit = {
    if (dataSource == null) {
      try {
        dataSource = DruidDataSourceFactory.createDataSource(druidProps)
      } catch {
        case e: Exception => {
          Log4jUtils.getLogger(this.getClass).error("数据库连接池初始化失败")
          throw new Exception(e)
        }
      }
    }
  }

  private def getConnection: java.sql.Connection = {
    initDataSource()
    val connection = dataSource.getConnection
    if (connection == null) {
      throw new Exception("数据库连接失败")
    }

    connection.setAutoCommit(false)
    connection
  }

  private def closeConnection(connection: java.sql.Connection): Unit = {
    if (connection != null && !connection.isClosed) {
      connection.close()
    }
  }

  private def executeUpdate(sql: String, para: Seq[Any], failComment: String): Int = {
    val connection = DBHelper.getConnection
    try {
      val ps = connection.prepareStatement(sql)
      for (i <- para.indices) {
        ps.setObject(i + 1, para(i))
      }
      val rows = ps.executeUpdate()
      connection.commit()
      rows
    } catch {
      case e: Exception => throw new Exception(failComment + e)
    } finally {
      DBHelper.closeConnection(connection)
    }
  }

  def add(sql: String, para: Any*): Int = {
    executeUpdate(sql, para, "添加失败")
  }

  def update(sql: String, para: Any*): Int = {
    executeUpdate(sql, para, "更新失败")
  }

  def delete(sql: String, para: Any*): Int = {
    executeUpdate(sql, para, "删除失败")
  }

  def query(sql: String, para: Any*): (java.sql.ResultSet, Connection) = {
    val connection = DBHelper.getConnection
    try {
      val ps = connection.prepareStatement(sql)
      for (i <- para.indices) {
        ps.setObject(i + 1, para(i))
      }
      (ps.executeQuery(), connection)
    } catch {
      case e: Exception =>
        DBHelper.closeConnection(connection)
        throw new Exception("查询失败" + e)
    }
  }
}
