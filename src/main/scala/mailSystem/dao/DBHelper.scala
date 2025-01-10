package mailSystem.dao

import com.alibaba.druid.pool.DruidDataSourceFactory
import mailSystem.utils.{Log4jUtils, MyUtils}

import java.sql.{Connection, SQLException, SQLIntegrityConstraintViolationException}
import java.util.Properties
import javax.sql.DataSource

object DBHelper {
  private var dataSource: DataSource = _
  private val druidProps = MyUtils.readConfig(new Properties(), "druid.properties")

  private val logger = Log4jUtils.getLogger(this.getClass)

  private def initDataSource(): Unit = {
    if (dataSource == null) {
      try {
        dataSource = DruidDataSourceFactory.createDataSource(druidProps)
      } catch {
        case e: Exception =>
          logger.error("数据库连接池初始化失败", e)
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
    try {
      atomicOperation(connection => {
        executeUpdateWithConnection(sql, para, connection)
      })
    } catch {
      case e: SQLException =>
        logger.error(failComment, e)
        throw new SQLException(e)
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
      case e: SQLException =>
        DBHelper.closeConnection(connection)
        logger.error("查询失败", e)
        throw new SQLException(e)
      case e: Exception =>
        DBHelper.closeConnection(connection)
        logger.error("查询失败", e)
        throw new Exception(e)
    }
  }

  def closeRsConn(rs: (java.sql.ResultSet, Connection)): Unit = {
    if (rs._1 != null) {
      rs._1.close()
    }
    DBHelper.closeConnection(rs._2)
  }

  private def executeUpdateWithConnection(sql: String, para: Seq[Any], connection: Connection): Int = {
    val ps = connection.prepareStatement(sql)
    try {
      for (i <- para.indices) {
        ps.setObject(i + 1, para(i))
      }
      ps.executeUpdate()
    } finally {
      ps.close()
    }
  }

  def addWithConnection(sql: String, para: Any*)(connection: Connection): Int = {
    executeUpdateWithConnection(sql, para, connection)
  }

  def updateWithConnection(sql: String, para: Any*)(connection: Connection): Int = {
    executeUpdateWithConnection(sql, para, connection)
  }

  def deleteWithConnection(sql: String, para: Any*)(connection: Connection): Int = {
    executeUpdateWithConnection(sql, para, connection)
  }

  def atomicOperation[T](call: Connection => T): T = {
    val connection = DBHelper.getConnection
    connection.setAutoCommit(false)
    try {
      val ret = call(connection)
      connection.commit()
      ret
    } catch {
      case e: SQLException if e.getSQLState == MyGlobalConfig.SQLERRORMAILCOUNTEXCEED =>
        connection.rollback()
        logger.error("事务执行失败，邮件数量超过限制", e)
        throw e
      case e: SQLIntegrityConstraintViolationException =>
        connection.rollback()
        logger.error("事务执行失败，违反完整性约束", e)
        throw e
      case e: Exception =>
        connection.rollback()
        throw new Exception("事务执行失败" + e)
    } finally {
      connection.setAutoCommit(true)
      DBHelper.closeConnection(connection)
    }
  }

  def atomicUpdate(paras: Seq[Seq[Any]]): Unit = {
    val connection = DBHelper.getConnection
    connection.setAutoCommit(false)
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
      connection.setAutoCommit(true)
      DBHelper.closeConnection(connection)
    }
  }
}
