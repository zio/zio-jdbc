package zio.jdbc

import zio.test._

import java.sql.{
  Connection,
  SQLXML,
  DatabaseMetaData,
  PreparedStatement,
  Struct,
  Blob,
  Statement,
  NClob,
  CallableStatement,
  Savepoint,
  SQLWarning,
  Clob
}
import java.util.{ Properties, concurrent }
import java.{ sql, util }
import java.io.{ InputStream, Reader }
import java.net.URL
import zio.{ ZIO, RuntimeFlags, Random }

object ZConnectionSpec extends ZIOSpecDefault {

  val createUsers: ZIO[ZConnectionPool with Any, Throwable, Unit] =
    transaction {
      sql"""
      create table users (
        id identity primary key,
        name varchar not null,
        age int not null
      )
      """.execute
    }

  def spec: Spec[TestEnvironment, Any] =
    suite("ZConnectionSpec TestConnection") {
      def testConnection = new ZConnection(new ZConnection.Restorable(new TestConnection))

      test("PreparedStatement Automatic Close Normal") {
        ZIO.scoped {
          for {
            statementClosedTuple <- testConnection.executeSqlWith(sql"""
                create table users_no_id (
                name varchar not null,
                age int not null
                )""")(ps => ZIO.succeed((ps, ps.isClosed())))
          } yield assertTrue(statementClosedTuple._1.isClosed() && !statementClosedTuple._2)
        }
      } +
        test("PreparedStatement Automatic Close Fail") {
          ZIO.scoped {
            for {
              statementClosedTuple <-
                testConnection
                  .executeSqlWith(sql"""
                create table users_no_id (
                name varchar not null,
                age int not null
                )""")(ps => ZIO.fail(new DummyException("Error Ocurred", ps, ps.isClosed())))
                  .catchSome { case e: DummyException => ZIO.succeed((e.preparedStatement, e.closedInScope)) }
            } yield assertTrue(statementClosedTuple._1.isClosed() && !statementClosedTuple._2)
          } //A bit of a hack, DummyException receives the prepared Statement so that its closed State can be checked outside ZConnection's Scope
        }
    } +
      suite("ZConnectionSpec LiveConnection") {

        def liveConnection = for {
          _       <- ZIO.attempt(Class.forName("org.h2.Driver"))
          int     <- Random.nextInt
          acquire <- ZIO.attemptBlocking {
                       java.sql.DriverManager.getConnection(s"jdbc:h2:mem:test_database_$int")
                     }
        } yield (new ZConnection(new ZConnection.Restorable(acquire)))

        test("PreparedStatement Automatic Close Normal") {
          ZIO.scoped {
            for {
              conn                 <- liveConnection
              statementClosedTuple <- conn.executeSqlWith(sql"""
                create table users_no_id (
                name varchar not null,
                age int not null
                )""")(ps => ZIO.succeed((ps.executeUpdate(), ps, ps.isClosed())))
            } yield assertTrue(
              statementClosedTuple._1 == 0 &&
                statementClosedTuple._2.isClosed() && !statementClosedTuple._3
            )
          }
        }
      }

  class DummyException(msg: String, val preparedStatement: PreparedStatement, val closedInScope: Boolean)
      extends Exception(msg)

  class TestConnection extends Connection { self =>

    private var closed               = false
    private var autoCommit           = true
    private var transactionIsolation = Connection.TRANSACTION_NONE
    private var catalog              = ""
    private var schema               = ""
    private var clientInfo           = new java.util.Properties()
    private var readOnly             = false

    def close(): Unit = closed = true

    def isClosed: Boolean = closed

    def createStatement(): Statement = ???

    def prepareStatement(sql: String): PreparedStatement = ???

    def prepareCall(sql: String): CallableStatement = ???

    def nativeSQL(sql: String): String = ???

    def setAutoCommit(autoCommit: Boolean): Unit = self.autoCommit = autoCommit

    def getAutoCommit: Boolean = autoCommit

    def commit(): Unit = ???

    def rollback(): Unit = ???

    def getMetaData: DatabaseMetaData = ???

    def setReadOnly(readOnly: Boolean): Unit = self.readOnly = readOnly

    def isReadOnly: Boolean = readOnly

    def setCatalog(catalog: String): Unit = self.catalog = catalog

    def getCatalog: String = catalog

    def setTransactionIsolation(level: RuntimeFlags): Unit = transactionIsolation = level

    def getTransactionIsolation: Int = transactionIsolation

    def getWarnings: SQLWarning = ???

    def clearWarnings(): Unit = ???

    def createStatement(resultSetType: RuntimeFlags, resultSetConcurrency: RuntimeFlags): Statement = ???

    def prepareStatement(
      sql: String,
      resultSetType: RuntimeFlags,
      resultSetConcurrency: RuntimeFlags
    ): PreparedStatement = ???

    def prepareCall(sql: String, resultSetType: RuntimeFlags, resultSetConcurrency: RuntimeFlags): CallableStatement =
      ???

    def getTypeMap: util.Map[String, Class[_]] = ???

    def setTypeMap(map: util.Map[String, Class[_]]): Unit = ???

    def setHoldability(holdability: RuntimeFlags): Unit = ???

    def getHoldability: RuntimeFlags = ???

    def setSavepoint(): Savepoint = ???

    def setSavepoint(name: String): Savepoint = ???

    def rollback(savepoint: Savepoint): Unit = ???

    def releaseSavepoint(savepoint: Savepoint): Unit = ???

    def createStatement(
      resultSetType: RuntimeFlags,
      resultSetConcurrency: RuntimeFlags,
      resultSetHoldability: RuntimeFlags
    ): Statement = ???

    def prepareStatement(
      sql: String,
      resultSetType: RuntimeFlags,
      resultSetConcurrency: RuntimeFlags,
      resultSetHoldability: RuntimeFlags
    ): PreparedStatement = ???

    def prepareCall(
      sql: String,
      resultSetType: RuntimeFlags,
      resultSetConcurrency: RuntimeFlags,
      resultSetHoldability: RuntimeFlags
    ): CallableStatement = ???

    def prepareStatement(sql: String, autoGeneratedKeys: RuntimeFlags): PreparedStatement = new DummyPreparedStatement

    def prepareStatement(sql: String, columnIndexes: Array[RuntimeFlags]): PreparedStatement = ???

    def prepareStatement(sql: String, columnNames: Array[String]): PreparedStatement = ???

    def createClob(): Clob = ???

    def createBlob(): Blob = ???

    def createNClob(): NClob = ???

    def createSQLXML(): SQLXML = ???

    def isValid(timeout: RuntimeFlags): Boolean = ???

    def setClientInfo(name: String, value: String): Unit = {
      val _ = clientInfo.setProperty(name, value)
    }

    def setClientInfo(properties: Properties): Unit = self.clientInfo = properties

    def getClientInfo(name: String): String = clientInfo.getProperty(name)

    def getClientInfo: Properties = clientInfo

    def createArrayOf(typeName: String, elements: Array[AnyRef]): sql.Array = ???

    def createStruct(typeName: String, attributes: Array[AnyRef]): Struct = ???

    def setSchema(schema: String): Unit = self.schema = schema

    def getSchema: String = schema

    def abort(executor: concurrent.Executor): Unit = ???

    def setNetworkTimeout(executor: concurrent.Executor, milliseconds: RuntimeFlags): Unit = ???

    def getNetworkTimeout: RuntimeFlags = ???

    def unwrap[T](iface: Class[T]): T = ???

    def isWrapperFor(iface: Class[_]): Boolean = ???
  }

  class DummyPreparedStatement() extends PreparedStatement {

    var closed = false

    override def unwrap[T <: Object](iface: Class[T]) = ???

    override def isWrapperFor(iface: Class[_ <: Object]) = ???

    override def executeQuery(sql: String) = ???

    override def executeUpdate(sql: String) = ???

    override def close() = closed = true

    override def getMaxFieldSize() = ???

    override def setMaxFieldSize(max: Int) = ???

    override def getMaxRows() = ???

    override def setMaxRows(max: Int) = ???

    override def setEscapeProcessing(enable: Boolean) = ???

    override def getQueryTimeout() = ???

    override def setQueryTimeout(seconds: Int) = ???

    override def cancel() = ???

    override def getWarnings() = ???

    override def clearWarnings() = ???

    override def setCursorName(name: String) = ???

    override def execute(sql: String): Boolean = ???

    override def getResultSet(): sql.ResultSet = ???

    override def getUpdateCount(): Int = ???

    override def getMoreResults(): Boolean = ???

    override def setFetchDirection(direction: Int): Unit = ???

    override def getFetchDirection(): Int = ???

    override def setFetchSize(rows: Int): Unit = ???

    override def getFetchSize(): Int = ???

    override def getResultSetConcurrency(): Int = ???

    override def getResultSetType(): Int = ???

    override def addBatch(sql: String): Unit = ???

    override def clearBatch(): Unit = ???

    override def executeBatch(): Array[Int] = ???

    override def getConnection(): Connection = ???

    override def getMoreResults(current: Int): Boolean = ???

    override def getGeneratedKeys(): sql.ResultSet = ???

    override def executeUpdate(sql: String, autoGeneratedKeys: Int): Int = ???

    override def executeUpdate(sql: String, columnIndexes: Array[Int]): Int = ???

    override def executeUpdate(sql: String, columnNames: Array[String]): Int = ???

    override def execute(sql: String, autoGeneratedKeys: Int): Boolean = ???

    override def execute(sql: String, columnIndexes: Array[Int]): Boolean = ???

    override def execute(sql: String, columnNames: Array[String]): Boolean = ???

    override def getResultSetHoldability(): Int = ???

    override def isClosed(): Boolean = closed

    override def setPoolable(poolable: Boolean): Unit = ???

    override def isPoolable(): Boolean = ???

    override def closeOnCompletion(): Unit = ???

    override def isCloseOnCompletion(): Boolean = ???

    override def executeQuery(): sql.ResultSet = ???

    override def executeUpdate(): Int = ???

    override def setNull(parameterIndex: Int, sqlType: Int): Unit = ???

    override def setBoolean(parameterIndex: Int, x: Boolean): Unit = ???

    override def setByte(parameterIndex: Int, x: Byte): Unit = ???

    override def setShort(parameterIndex: Int, x: Short): Unit = ???

    override def setInt(parameterIndex: Int, x: Int): Unit = ???

    override def setLong(parameterIndex: Int, x: Long): Unit = ???

    override def setFloat(parameterIndex: Int, x: Float): Unit = ???

    override def setDouble(parameterIndex: Int, x: Double): Unit = ???

    override def setBigDecimal(parameterIndex: Int, x: java.math.BigDecimal): Unit = ???

    override def setString(parameterIndex: Int, x: String): Unit = ???

    override def setBytes(parameterIndex: Int, x: Array[Byte]): Unit = ???

    override def setDate(parameterIndex: Int, x: sql.Date): Unit = ???

    override def setTime(parameterIndex: Int, x: sql.Time): Unit = ???

    override def setTimestamp(parameterIndex: Int, x: sql.Timestamp): Unit = ???

    override def setAsciiStream(parameterIndex: Int, x: InputStream, length: Int): Unit = ???

    override def setUnicodeStream(parameterIndex: Int, x: InputStream, length: Int): Unit = ???

    override def setBinaryStream(parameterIndex: Int, x: InputStream, length: Int): Unit = ???

    override def clearParameters(): Unit = ???

    override def setObject(parameterIndex: Int, x: Object, targetSqlType: Int): Unit = ???

    override def setObject(parameterIndex: Int, x: Object): Unit = ???

    override def execute(): Boolean = ???

    override def addBatch(): Unit = ???

    override def setCharacterStream(parameterIndex: Int, reader: Reader, length: Int): Unit = ???

    override def setRef(parameterIndex: Int, x: sql.Ref): Unit = ???

    override def setBlob(parameterIndex: Int, x: Blob): Unit = ???

    override def setClob(parameterIndex: Int, x: Clob): Unit = ???

    override def setArray(parameterIndex: Int, x: sql.Array): Unit = ???

    override def getMetaData(): sql.ResultSetMetaData = ???

    override def setDate(parameterIndex: Int, x: sql.Date, cal: util.Calendar): Unit = ???

    override def setTime(parameterIndex: Int, x: sql.Time, cal: util.Calendar): Unit = ???

    override def setTimestamp(parameterIndex: Int, x: sql.Timestamp, cal: util.Calendar): Unit = ???

    override def setNull(parameterIndex: Int, sqlType: Int, typeName: String): Unit = ???

    override def setURL(parameterIndex: Int, x: URL): Unit = ???

    override def getParameterMetaData(): sql.ParameterMetaData = ???

    override def setRowId(parameterIndex: Int, x: sql.RowId): Unit = ???

    override def setNString(parameterIndex: Int, value: String): Unit = ???

    override def setNCharacterStream(parameterIndex: Int, value: Reader, length: Long): Unit = ???

    override def setNClob(parameterIndex: Int, value: NClob): Unit = ???

    override def setClob(parameterIndex: Int, reader: Reader, length: Long): Unit = ???

    override def setBlob(parameterIndex: Int, inputStream: InputStream, length: Long): Unit = ???

    override def setNClob(parameterIndex: Int, reader: Reader, length: Long): Unit = ???

    override def setSQLXML(parameterIndex: Int, xmlObject: SQLXML): Unit = ???

    override def setObject(parameterIndex: Int, x: Object, targetSqlType: Int, scaleOrLength: Int): Unit = ???

    override def setAsciiStream(parameterIndex: Int, x: InputStream, length: Long): Unit = ???

    override def setBinaryStream(parameterIndex: Int, x: InputStream, length: Long): Unit = ???

    override def setCharacterStream(parameterIndex: Int, reader: Reader, length: Long): Unit = ???

    override def setAsciiStream(parameterIndex: Int, x: InputStream): Unit = ???

    override def setBinaryStream(parameterIndex: Int, x: InputStream): Unit = ???

    override def setCharacterStream(parameterIndex: Int, reader: Reader): Unit = ???

    override def setNCharacterStream(parameterIndex: Int, value: Reader): Unit = ???

    override def setClob(parameterIndex: Int, reader: Reader): Unit = ???

    override def setBlob(parameterIndex: Int, inputStream: InputStream): Unit = ???

    override def setNClob(parameterIndex: Int, reader: Reader): Unit = ???

  }

}
