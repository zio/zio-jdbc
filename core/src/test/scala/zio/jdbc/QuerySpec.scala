package zio.jdbc

import zio._
import zio.test._
import scala.util

import java.sql.{
  Blob,
  CallableStatement,
  Clob,
  Connection,
  DatabaseMetaData,
  NClob,
  PreparedStatement,
  SQLWarning,
  SQLXML,
  Savepoint,
  Statement,
  Struct
}
import java.util.{ Properties, concurrent }
import java.{ sql, util }
import java.io.{ InputStream, Reader }
import java.net.URL

object QuerySpec extends ZIOSpecDefault {

  final case class User(name: String, age: Int)

  object User {
    implicit val jdbcDecoder: JdbcDecoder[User] =
      JdbcDecoder[(String, Int)]().map[User](t => User(t._1, t._2))

    implicit val jdbcEncoder: JdbcEncoder[User] = (value: User) => {
      val name = value.name
      val age  = value.age
      sql"""${name}""" ++ ", " ++ s"${age}"
    }
  }

  final case class UserNoId(name: String, age: Int)

  object UserNoId {
    implicit val jdbcDecoder: JdbcDecoder[UserNoId] =
      JdbcDecoder[(String, Int)]().map[UserNoId](t => UserNoId(t._1, t._2))

    implicit val jdbcEncoder: JdbcEncoder[UserNoId] = (value: UserNoId) => {
      val name = value.name
      val age  = value.age
      sql"""${name}""" ++ ", " ++ s"${age}"
    }
  }

  def createTableUsers =
    sql"""
          create table users (
            id identity primary key,
            name varchar not null,
            age int not null
            )""".execute

  def genUser: UserNoId = {
    val name = scala.util.Random.nextString(8)
    val id   = scala.util.Random.nextInt(100000)
    UserNoId(name, id)
  }

  def genUsers(size: Int): List[UserNoId] = List.fill(size)(genUser)

  def insertEverything: ZIO[ZConnection, Throwable, Long] = {
    val users           = genUsers(3000)
    val insertStatement = SqlFragment.insertInto("users")("name", "age").values(users)
    for {
      inserted <- insertStatement.insert
    } yield inserted.rowsUpdated
  }

  def spec: Spec[TestEnvironment, Any] =
    suite("QuerySpec Unit") {
      def testConnection     = new ZConnection(new ZConnection.Restorable(new TestConnection))
      val tableName          = sql"users"
      val fields             = SqlFragment("id, name, age")
      val testSql            = sql"select $fields from $tableName"
      val query: Query[User] = testSql.query[User]
      test("Query Success ResultSet Automatic Close") {
        ZIO.scoped {
          for {
            rsClosedTuple <- ZIO.scoped {
                               for {
                                 rs     <- query.executeQuery(testSql)
                                 closed <- rs.access(_.isClosed())
                               } yield (rs, closed)
                             }
            closed        <- rsClosedTuple._1.access(_.isClosed())
          } yield assertTrue(closed && !rsClosedTuple._2)
        }.provide(ZLayer.succeed(testConnection))
      } +
        test("Query ResultSet Iteration Fail Automatic Close") {
          assertTrue(false)
        } +
        test("Query ResultSet excecute Fail Automatic Close") {
          assertTrue(false)
        }
    } +
      suite("QuerySpec Integration") {
        def liveConnection =
          for {
            _       <- ZIO.attempt(Class.forName("org.h2.Driver"))
            int     <- Random.nextInt
            acquire <- ZIO.attemptBlocking {
                         java.sql.DriverManager.getConnection(s"jdbc:h2:mem:test_database_$int")
                       }
          } yield (new ZConnection(new ZConnection.Restorable(acquire)))

        test("Query Success ResultSet Automatic Close") {

          ZIO.scoped {

            val tableName          = sql"users"
            val fields             = SqlFragment("id, name, age")
            val testSql            = sql"select $fields from $tableName"
            val query: Query[User] = testSql.query[User]
            for {
              _             <- createTableUsers
              _             <- insertEverything
              rsClosedTuple <- ZIO.scoped {
                                 for {
                                   rs     <- query.executeQuery(testSql)
                                   closed <- rs.access(_.isClosed())
                                 } yield (rs, closed)
                               }
              closed        <- rsClosedTuple._1.access(_.isClosed())
            } yield assertTrue(
              closed && !rsClosedTuple._2
            ) //Assert ResultSet is closed Outside scope but was open inside scope
          }.provide(ZLayer.fromZIO(liveConnection))
        }
      }

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

    def getTypeMap: java.util.Map[String, Class[_]] = ???

    def setTypeMap(map: java.util.Map[String, Class[_]]): Unit = ???

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

    override def unwrap[T <: Object](iface: Class[T]) = ???

    override def isWrapperFor(iface: Class[_ <: Object]) = ???

    override def executeQuery(sql: String) = ???

    override def executeUpdate(sql: String) = ???

    override def close() = ???

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

    override def isClosed(): Boolean = ???

    override def setPoolable(poolable: Boolean): Unit = ???

    override def isPoolable(): Boolean = ???

    override def closeOnCompletion(): Unit = ???

    override def isCloseOnCompletion(): Boolean = ???

    override def executeQuery(): sql.ResultSet = new DummyResultSet

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

    override def setDate(parameterIndex: Int, x: sql.Date, cal: java.util.Calendar): Unit = ???

    override def setTime(parameterIndex: Int, x: sql.Time, cal: java.util.Calendar): Unit = ???

    override def setTimestamp(parameterIndex: Int, x: sql.Timestamp, cal: java.util.Calendar): Unit = ???

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

  class DummyResultSet extends sql.ResultSet {

    var closed = false

    override def unwrap[T <: Object](x$1: Class[T]): T = ???

    override def isWrapperFor(x$1: Class[_ <: Object]): Boolean = ???

    override def next(): Boolean = ???

    override def close(): Unit = closed = true

    override def wasNull(): Boolean = ???

    override def getString(columnIndex: Int): String = ???

    override def getBoolean(columnIndex: Int): Boolean = ???

    override def getByte(columnIndex: Int): Byte = ???

    override def getShort(columnIndex: Int): Short = ???

    override def getInt(columnIndex: Int): Int = ???

    override def getLong(columnIndex: Int): Long = ???

    override def getFloat(columnIndex: Int): Float = ???

    override def getDouble(columnIndex: Int): Double = ???

    override def getBigDecimal(columnIndex: Int, scale: Int): java.math.BigDecimal = ???

    override def getBytes(columnIndex: Int): Array[Byte] = ???

    override def getDate(columnIndex: Int): sql.Date = ???

    override def getTime(columnIndex: Int): sql.Time = ???

    override def getTimestamp(columnIndex: Int): sql.Timestamp = ???

    override def getAsciiStream(columnIndex: Int): InputStream = ???

    override def getUnicodeStream(columnIndex: Int): InputStream = ???

    override def getBinaryStream(columnIndex: Int): InputStream = ???

    override def getString(columnLabel: String): String = ???

    override def getBoolean(columnLabel: String): Boolean = ???

    override def getByte(columnLabel: String): Byte = ???

    override def getShort(columnLabel: String): Short = ???

    override def getInt(columnLabel: String): Int = ???

    override def getLong(columnLabel: String): Long = ???

    override def getFloat(columnLabel: String): Float = ???

    override def getDouble(columnLabel: String): Double = ???

    override def getBigDecimal(columnLabel: String, scale: Int): java.math.BigDecimal = ???

    override def getBytes(columnLabel: String): Array[Byte] = ???

    override def getDate(columnLabel: String): sql.Date = ???

    override def getTime(columnLabel: String): sql.Time = ???

    override def getTimestamp(columnLabel: String): sql.Timestamp = ???

    override def getAsciiStream(columnLabel: String): InputStream = ???

    override def getUnicodeStream(columnLabel: String): InputStream = ???

    override def getBinaryStream(columnLabel: String): InputStream = ???

    override def getWarnings(): SQLWarning = ???

    override def clearWarnings(): Unit = ???

    override def getCursorName(): String = ???

    override def getMetaData(): sql.ResultSetMetaData = ???

    override def getObject(columnIndex: Int): Object = ???

    override def getObject(columnLabel: String): Object = ???

    override def findColumn(columnLabel: String): Int = ???

    override def getCharacterStream(columnIndex: Int): Reader = ???

    override def getCharacterStream(columnLabel: String): Reader = ???

    override def getBigDecimal(columnIndex: Int): java.math.BigDecimal = ???

    override def getBigDecimal(columnLabel: String): java.math.BigDecimal = ???

    override def isBeforeFirst(): Boolean = ???

    override def isAfterLast(): Boolean = ???

    override def isFirst(): Boolean = ???

    override def isLast(): Boolean = ???

    override def beforeFirst(): Unit = ???

    override def afterLast(): Unit = ???

    override def first(): Boolean = ???

    override def last(): Boolean = ???

    override def getRow(): Int = ???

    override def absolute(row: Int): Boolean = ???

    override def relative(rows: Int): Boolean = ???

    override def previous(): Boolean = ???

    override def setFetchDirection(direction: Int): Unit = ???

    override def getFetchDirection(): Int = ???

    override def setFetchSize(rows: Int): Unit = ???

    override def getFetchSize(): Int = ???

    override def getType(): Int = ???

    override def getConcurrency(): Int = ???

    override def rowUpdated(): Boolean = ???

    override def rowInserted(): Boolean = ???

    override def rowDeleted(): Boolean = ???

    override def updateNull(columnIndex: Int): Unit = ???

    override def updateBoolean(columnIndex: Int, x: Boolean): Unit = ???

    override def updateByte(columnIndex: Int, x: Byte): Unit = ???

    override def updateShort(columnIndex: Int, x: Short): Unit = ???

    override def updateInt(columnIndex: Int, x: Int): Unit = ???

    override def updateLong(columnIndex: Int, x: Long): Unit = ???

    override def updateFloat(columnIndex: Int, x: Float): Unit = ???

    override def updateDouble(columnIndex: Int, x: Double): Unit = ???

    override def updateBigDecimal(columnIndex: Int, x: java.math.BigDecimal): Unit = ???

    override def updateString(columnIndex: Int, x: String): Unit = ???

    override def updateBytes(columnIndex: Int, x: Array[Byte]): Unit = ???

    override def updateDate(columnIndex: Int, x: sql.Date): Unit = ???

    override def updateTime(columnIndex: Int, x: sql.Time): Unit = ???

    override def updateTimestamp(columnIndex: Int, x: sql.Timestamp): Unit = ???

    override def updateAsciiStream(columnIndex: Int, x: InputStream, length: Int): Unit = ???

    override def updateBinaryStream(columnIndex: Int, x: InputStream, length: Int): Unit = ???

    override def updateCharacterStream(columnIndex: Int, x: Reader, length: Int): Unit = ???

    override def updateObject(columnIndex: Int, x: Object, scaleOrLength: Int): Unit = ???

    override def updateObject(columnIndex: Int, x: Object): Unit = ???

    override def updateNull(columnLabel: String): Unit = ???

    override def updateBoolean(columnLabel: String, x: Boolean): Unit = ???

    override def updateByte(columnLabel: String, x: Byte): Unit = ???

    override def updateShort(columnLabel: String, x: Short): Unit = ???

    override def updateInt(columnLabel: String, x: Int): Unit = ???

    override def updateLong(columnLabel: String, x: Long): Unit = ???

    override def updateFloat(columnLabel: String, x: Float): Unit = ???

    override def updateDouble(columnLabel: String, x: Double): Unit = ???

    override def updateBigDecimal(columnLabel: String, x: java.math.BigDecimal): Unit = ???

    override def updateString(columnLabel: String, x: String): Unit = ???

    override def updateBytes(columnLabel: String, x: Array[Byte]): Unit = ???

    override def updateDate(columnLabel: String, x: sql.Date): Unit = ???

    override def updateTime(columnLabel: String, x: sql.Time): Unit = ???

    override def updateTimestamp(columnLabel: String, x: sql.Timestamp): Unit = ???

    override def updateAsciiStream(columnLabel: String, x: InputStream, length: Int): Unit = ???

    override def updateBinaryStream(columnLabel: String, x: InputStream, length: Int): Unit = ???

    override def updateCharacterStream(columnLabel: String, reader: Reader, length: Int): Unit = ???

    override def updateObject(columnLabel: String, x: Object, scaleOrLength: Int): Unit = ???

    override def updateObject(columnLabel: String, x: Object): Unit = ???

    override def insertRow(): Unit = ???

    override def updateRow(): Unit = ???

    override def deleteRow(): Unit = ???

    override def refreshRow(): Unit = ???

    override def cancelRowUpdates(): Unit = ???

    override def moveToInsertRow(): Unit = ???

    override def moveToCurrentRow(): Unit = ???

    override def getStatement(): Statement = ???

    override def getObject(columnIndex: Int, map: java.util.Map[String, Class[_ <: Object]]): Object = ???

    override def getRef(columnIndex: Int): sql.Ref = ???

    override def getBlob(columnIndex: Int): Blob = ???

    override def getClob(columnIndex: Int): Clob = ???

    override def getArray(columnIndex: Int): sql.Array = ???

    override def getObject(columnLabel: String, map: java.util.Map[String, Class[_ <: Object]]): Object = ???

    override def getRef(columnLabel: String): sql.Ref = ???

    override def getBlob(columnLabel: String): Blob = ???

    override def getClob(columnLabel: String): Clob = ???

    override def getArray(columnLabel: String): sql.Array = ???

    override def getDate(columnIndex: Int, cal: java.util.Calendar): sql.Date = ???

    override def getDate(columnLabel: String, cal: java.util.Calendar): sql.Date = ???

    override def getTime(columnIndex: Int, cal: java.util.Calendar): sql.Time = ???

    override def getTime(columnLabel: String, cal: java.util.Calendar): sql.Time = ???

    override def getTimestamp(columnIndex: Int, cal: java.util.Calendar): sql.Timestamp = ???

    override def getTimestamp(columnLabel: String, cal: java.util.Calendar): sql.Timestamp = ???

    override def getURL(columnIndex: Int): URL = ???

    override def getURL(columnLabel: String): URL = ???

    override def updateRef(columnIndex: Int, x: sql.Ref): Unit = ???

    override def updateRef(columnLabel: String, x: sql.Ref): Unit = ???

    override def updateBlob(columnIndex: Int, x: Blob): Unit = ???

    override def updateBlob(columnLabel: String, x: Blob): Unit = ???

    override def updateClob(columnIndex: Int, x: Clob): Unit = ???

    override def updateClob(columnLabel: String, x: Clob): Unit = ???

    override def updateArray(columnIndex: Int, x: sql.Array): Unit = ???

    override def updateArray(columnLabel: String, x: sql.Array): Unit = ???

    override def getRowId(columnIndex: Int): sql.RowId = ???

    override def getRowId(columnLabel: String): sql.RowId = ???

    override def updateRowId(columnIndex: Int, x: sql.RowId): Unit = ???

    override def updateRowId(columnLabel: String, x: sql.RowId): Unit = ???

    override def getHoldability(): Int = ???

    override def isClosed(): Boolean = closed

    override def updateNString(columnIndex: Int, nString: String): Unit = ???

    override def updateNString(columnLabel: String, nString: String): Unit = ???

    override def updateNClob(columnIndex: Int, nClob: NClob): Unit = ???

    override def updateNClob(columnLabel: String, nClob: NClob): Unit = ???

    override def getNClob(columnIndex: Int): NClob = ???

    override def getNClob(columnLabel: String): NClob = ???

    override def getSQLXML(columnIndex: Int): SQLXML = ???

    override def getSQLXML(columnLabel: String): SQLXML = ???

    override def updateSQLXML(columnIndex: Int, xmlObject: SQLXML): Unit = ???

    override def updateSQLXML(columnLabel: String, xmlObject: SQLXML): Unit = ???

    override def getNString(columnIndex: Int): String = ???

    override def getNString(columnLabel: String): String = ???

    override def getNCharacterStream(columnIndex: Int): Reader = ???

    override def getNCharacterStream(columnLabel: String): Reader = ???

    override def updateNCharacterStream(columnIndex: Int, x: Reader, length: Long): Unit = ???

    override def updateNCharacterStream(columnLabel: String, reader: Reader, length: Long): Unit = ???

    override def updateAsciiStream(columnIndex: Int, x: InputStream, length: Long): Unit = ???

    override def updateBinaryStream(columnIndex: Int, x: InputStream, length: Long): Unit = ???

    override def updateCharacterStream(columnIndex: Int, x: Reader, length: Long): Unit = ???

    override def updateAsciiStream(columnLabel: String, x: InputStream, length: Long): Unit = ???

    override def updateBinaryStream(columnLabel: String, x: InputStream, length: Long): Unit = ???

    override def updateCharacterStream(columnLabel: String, reader: Reader, length: Long): Unit = ???

    override def updateBlob(columnIndex: Int, inputStream: InputStream, length: Long): Unit = ???

    override def updateBlob(columnLabel: String, inputStream: InputStream, length: Long): Unit = ???

    override def updateClob(columnIndex: Int, reader: Reader, length: Long): Unit = ???

    override def updateClob(columnLabel: String, reader: Reader, length: Long): Unit = ???

    override def updateNClob(columnIndex: Int, reader: Reader, length: Long): Unit = ???

    override def updateNClob(columnLabel: String, reader: Reader, length: Long): Unit = ???

    override def updateNCharacterStream(columnIndex: Int, x: Reader): Unit = ???

    override def updateNCharacterStream(columnLabel: String, reader: Reader): Unit = ???

    override def updateAsciiStream(columnIndex: Int, x: InputStream): Unit = ???

    override def updateBinaryStream(columnIndex: Int, x: InputStream): Unit = ???

    override def updateCharacterStream(columnIndex: Int, x: Reader): Unit = ???

    override def updateAsciiStream(columnLabel: String, x: InputStream): Unit = ???

    override def updateBinaryStream(columnLabel: String, x: InputStream): Unit = ???

    override def updateCharacterStream(columnLabel: String, reader: Reader): Unit = ???

    override def updateBlob(columnIndex: Int, inputStream: InputStream): Unit = ???

    override def updateBlob(columnLabel: String, inputStream: InputStream): Unit = ???

    override def updateClob(columnIndex: Int, reader: Reader): Unit = ???

    override def updateClob(columnLabel: String, reader: Reader): Unit = ???

    override def updateNClob(columnIndex: Int, reader: Reader): Unit = ???

    override def updateNClob(columnLabel: String, reader: Reader): Unit = ???

    override def getObject[T <: Object](columnIndex: Int, `type`: Class[T]): T = ???

    override def getObject[T <: Object](columnLabel: String, `type`: Class[T]): T = ???

  }

}
