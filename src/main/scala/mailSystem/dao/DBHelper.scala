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
        case e: Exception =>
          Log4jUtils.getLogger(this.getClass).error("数据库连接池初始化失败")
          throw new Exception(e)
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
      var rows = 0
      val ps = connection.prepareStatement(sql)
      try {
        for (i <- para.indices) {
          ps.setObject(i + 1, para(i))
        }
        rows = ps.executeUpdate()
      } finally {
        ps.close()
      }
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
      try {
        for (i <- para.indices) {
          ps.setObject(i + 1, para(i))
        }
        (ps.executeQuery(), connection)
      } finally {
        ps.close()
      }
    } catch {
      case e: Exception =>
        DBHelper.closeConnection(connection)
        throw new Exception("查询失败" + e)
    }
  }

  def closeRsConn(rs: (java.sql.ResultSet, Connection)): Unit = {
    if (rs._1 != null) {
      rs._1.close()
    }
    DBHelper.closeConnection(rs._2)
  }

  def atomicUpdate(paras: Seq[Seq[Any]]): Unit = {
    val connection = DBHelper.getConnection
    try {
      for (i <- paras.indices) {
        val ps = connection.prepareStatement(paras(i).head.asInstanceOf[String])
        try {
          for (j <- 1 until paras(i).length) {
            ps.setObject(j, paras(i)(j))
          }
          ps.executeUpdate()
        } finally {
          ps.close()
        }
      }
      connection.commit()
    } catch {
      case e: Exception =>
        connection.rollback()
        throw new Exception("事务执行失败" + e)
    } finally {
      DBHelper.closeConnection(connection)
    }
  }
}
