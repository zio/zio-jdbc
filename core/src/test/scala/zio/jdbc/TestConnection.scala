package zio.jdbc

import zio.RuntimeFlags

import java.io.{ InputStream, Reader }
import java.net.URL
import java.sql.{
  Blob,
  CallableStatement,
  Clob,
  Connection,
  DatabaseMetaData,
  NClob,
  PreparedStatement,
  ResultSet,
  SQLWarning,
  SQLXML,
  Savepoint,
  Statement,
  Struct
}
import java.util.{ Properties, concurrent }
import java.{ sql, util }

class TestConnection(failNext: Boolean = false, elems: Int = 0) extends Connection { self =>

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

  def prepareStatement(sql: String): PreparedStatement = new DummyPreparedStatement(failNext, elems)

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

  def prepareStatement(sql: String, autoGeneratedKeys: RuntimeFlags): PreparedStatement =
    new DummyPreparedStatement(failNext, elems)

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

class DummyPreparedStatement(failNext: Boolean, elemns: Int) extends PreparedStatement {

  var closed = false

  override def unwrap[T](iface: Class[T]): T = ???

  override def isWrapperFor(iface: Class[_]): Boolean = ???

  override def executeQuery(sql: String) = new DummyResultSet(failNext, elemns)

  override def executeUpdate(sql: String) = ???

  override def close(): Unit = closed = true

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

  override def getGeneratedKeys(): sql.ResultSet = new DummyResultSet(failNext, elemns)

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

  override def executeQuery(): ResultSet = new DummyResultSet(failNext, elemns)

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

  override def executeLargeUpdate(): Long = 0

}

class DummyResultSet(failNext: Boolean, elems: Int) extends ResultSet {

  var closed      = false
  var currentElem = 0

  override def unwrap[T](x$1: Class[T]): T = ???

  override def isWrapperFor(x$1: Class[_]): Boolean = ???

  override def next(): Boolean =
    if (failNext) {
      throw new sql.SQLException()
    } else if (currentElem < elems) {
      currentElem += 1
      true
    } else {
      false
    }

  override def close(): Unit = closed = true

  override def wasNull() = ???

  override def getString(columnIndex: Int) = ???

  override def getBoolean(columnIndex: Int) = ???

  override def getByte(columnIndex: Int) = ???

  override def getShort(columnIndex: Int) = ???

  override def getInt(columnIndex: Int) = ???

  override def getLong(columnIndex: Int) = ???

  override def getFloat(columnIndex: Int) = ???

  override def getDouble(columnIndex: Int) = ???

  override def getBigDecimal(columnIndex: Int, scale: Int) = ???

  override def getBytes(columnIndex: Int) = ???

  override def getDate(columnIndex: Int) = ???

  override def getTime(columnIndex: Int) = ???

  override def getTimestamp(columnIndex: Int) = ???

  override def getAsciiStream(columnIndex: Int) = ???

  override def getUnicodeStream(columnIndex: Int) = ???

  override def getBinaryStream(columnIndex: Int) = ???

  override def getString(columnLabel: String) = ???

  override def getBoolean(columnLabel: String) = ???

  override def getByte(columnLabel: String) = ???

  override def getShort(columnLabel: String) = ???

  override def getInt(columnLabel: String) = ???

  override def getLong(columnLabel: String) = ???

  override def getFloat(columnLabel: String) = ???

  override def getDouble(columnLabel: String) = ???

  override def getBigDecimal(columnLabel: String, scale: Int) = ???

  override def getBytes(columnLabel: String) = ???

  override def getDate(columnLabel: String) = ???

  override def getTime(columnLabel: String) = ???

  override def getTimestamp(columnLabel: String) = ???

  override def getAsciiStream(columnLabel: String) = ???

  override def getUnicodeStream(columnLabel: String) = ???

  override def getBinaryStream(columnLabel: String) = ???

  override def getWarnings() = ???

  override def clearWarnings() = ???

  override def getCursorName() = ???

  override def getMetaData() = ???

  override def getObject(columnIndex: Int) = ???

  override def getObject(columnLabel: String) = ???

  override def findColumn(columnLabel: String) = ???

  override def getCharacterStream(columnIndex: Int) = ???

  override def getCharacterStream(columnLabel: String) = ???

  override def getBigDecimal(columnIndex: Int) = ???

  override def getBigDecimal(columnLabel: String) = ???

  override def isBeforeFirst() = ???

  override def isAfterLast() = ???

  override def isFirst() = ???

  override def isLast() = ???

  override def beforeFirst() = ???

  override def afterLast() = ???

  override def first() = ???

  override def last() = ???

  override def getRow() = ???

  override def absolute(row: Int) = ???

  override def relative(rows: Int) = ???

  override def previous() = ???

  override def setFetchDirection(direction: Int) = ???

  override def getFetchDirection() = ???

  override def setFetchSize(rows: Int) = ???

  override def getFetchSize() = ???

  override def getType() = ???

  override def getConcurrency() = ???

  override def rowUpdated() = ???

  override def rowInserted() = ???

  override def rowDeleted() = ???

  override def updateNull(columnIndex: Int) = ???

  override def updateBoolean(columnIndex: Int, x: Boolean) = ???

  override def updateByte(columnIndex: Int, x: Byte) = ???

  override def updateShort(columnIndex: Int, x: Short) = ???

  override def updateInt(columnIndex: Int, x: Int) = ???

  override def updateLong(columnIndex: Int, x: Long) = ???

  override def updateFloat(columnIndex: Int, x: Float) = ???

  override def updateDouble(columnIndex: Int, x: Double): Unit = ???

  override def updateBigDecimal(columnIndex: Int, x: java.math.BigDecimal) = ???

  override def updateString(columnIndex: Int, x: String) = ???

  override def updateBytes(columnIndex: Int, x: Array[Byte]) = ???

  override def updateDate(columnIndex: Int, x: sql.Date) = ???

  override def updateTime(columnIndex: Int, x: sql.Time) = ???

  override def updateTimestamp(columnIndex: Int, x: sql.Timestamp) = ???

  override def updateAsciiStream(columnIndex: Int, x: InputStream, length: Int) = ???

  override def updateBinaryStream(columnIndex: Int, x: InputStream, length: Int) = ???

  override def updateCharacterStream(columnIndex: Int, x: Reader, length: Int) = ???

  override def updateObject(columnIndex: Int, x: Object, scaleOrLength: Int) = ???

  override def updateObject(columnIndex: Int, x: Object) = ???

  override def updateNull(columnLabel: String) = ???

  override def updateBoolean(columnLabel: String, x: Boolean) = ???

  override def updateByte(columnLabel: String, x: Byte) = ???

  override def updateShort(columnLabel: String, x: Short) = ???

  override def updateInt(columnLabel: String, x: Int) = ???

  override def updateLong(columnLabel: String, x: Long) = ???

  override def updateFloat(columnLabel: String, x: Float) = ???

  override def updateDouble(columnLabel: String, x: Double) = ???

  override def updateBigDecimal(columnLabel: String, x: java.math.BigDecimal) = ???

  override def updateString(columnLabel: String, x: String) = ???

  override def updateBytes(columnLabel: String, x: Array[Byte]) = ???

  override def updateDate(columnLabel: String, x: sql.Date) = ???

  override def updateTime(columnLabel: String, x: sql.Time) = ???

  override def updateTimestamp(columnLabel: String, x: sql.Timestamp) = ???

  override def updateAsciiStream(columnLabel: String, x: InputStream, length: Int) = ???

  override def updateBinaryStream(columnLabel: String, x: InputStream, length: Int) = ???

  override def updateCharacterStream(columnLabel: String, reader: Reader, length: Int) = ???

  override def updateObject(columnLabel: String, x: Object, scaleOrLength: Int) = ???

  override def updateObject(columnLabel: String, x: Object) = ???

  override def insertRow() = ???

  override def updateRow() = ???

  override def deleteRow() = ???

  override def refreshRow() = ???

  override def cancelRowUpdates() = ???

  override def moveToInsertRow() = ???

  override def moveToCurrentRow() = ???

  override def getStatement() = ???

  override def getObject(columnIndex: Int, map: util.Map[String, Class[_ <: Object]]) = ???

  override def getRef(columnIndex: Int) = ???

  override def getBlob(columnIndex: Int) = ???

  override def getClob(columnIndex: Int) = ???

  override def getArray(columnIndex: Int) = ???

  override def getObject(columnLabel: String, map: util.Map[String, Class[_ <: Object]]) = ???

  override def getRef(columnLabel: String) = ???

  override def getBlob(columnLabel: String) = ???

  override def getClob(columnLabel: String) = ???

  override def getArray(columnLabel: String) = ???

  override def getDate(columnIndex: Int, cal: java.util.Calendar) = ???

  override def getDate(columnLabel: String, cal: java.util.Calendar) = ???

  override def getTime(columnIndex: Int, cal: java.util.Calendar) = ???

  override def getTime(columnLabel: String, cal: java.util.Calendar) = ???

  override def getTimestamp(columnIndex: Int, cal: java.util.Calendar) = ???

  override def getTimestamp(columnLabel: String, cal: java.util.Calendar) = ???

  override def getURL(columnIndex: Int) = ???

  override def getURL(columnLabel: String) = ???

  override def updateRef(columnIndex: Int, x: sql.Ref) = ???

  override def updateRef(columnLabel: String, x: sql.Ref) = ???

  override def updateBlob(columnIndex: Int, x: Blob) = ???

  override def updateBlob(columnLabel: String, x: Blob) = ???

  override def updateClob(columnIndex: Int, x: Clob) = ???

  override def updateClob(columnLabel: String, x: Clob) = ???

  override def updateArray(columnIndex: Int, x: sql.Array) = ???

  override def updateArray(columnLabel: String, x: sql.Array) = ???

  override def getRowId(columnIndex: Int) = ???

  override def getRowId(columnLabel: String) = ???

  override def updateRowId(columnIndex: Int, x: sql.RowId) = ???

  override def updateRowId(columnLabel: String, x: sql.RowId) = ???

  override def getHoldability() = ???

  override def isClosed() = closed

  override def updateNString(columnIndex: Int, nString: String) = ???

  override def updateNString(columnLabel: String, nString: String) = ???

  override def updateNClob(columnIndex: Int, nClob: NClob) = ???

  override def updateNClob(columnLabel: String, nClob: NClob) = ???

  override def getNClob(columnIndex: Int) = ???

  override def getNClob(columnLabel: String) = ???

  override def getSQLXML(columnIndex: Int) = ???

  override def getSQLXML(columnLabel: String) = ???

  override def updateSQLXML(columnIndex: Int, xmlObject: SQLXML) = ???

  override def updateSQLXML(columnLabel: String, xmlObject: SQLXML) = ???

  override def getNString(columnIndex: Int) = ???

  override def getNString(columnLabel: String) = ???

  override def getNCharacterStream(columnIndex: Int) = ???

  override def getNCharacterStream(columnLabel: String) = ???

  override def updateNCharacterStream(columnIndex: Int, x: Reader, length: Long) = ???

  override def updateNCharacterStream(columnLabel: String, reader: Reader, length: Long) = ???

  override def updateAsciiStream(columnIndex: Int, x: InputStream, length: Long) = ???

  override def updateBinaryStream(columnIndex: Int, x: InputStream, length: Long) = ???

  override def updateCharacterStream(columnIndex: Int, x: Reader, length: Long) = ???

  override def updateAsciiStream(columnLabel: String, x: InputStream, length: Long) = ???

  override def updateBinaryStream(columnLabel: String, x: InputStream, length: Long) = ???

  override def updateCharacterStream(columnLabel: String, reader: Reader, length: Long) = ???

  override def updateBlob(columnIndex: Int, inputStream: InputStream, length: Long) = ???

  override def updateBlob(columnLabel: String, inputStream: InputStream, length: Long) = ???

  override def updateClob(columnIndex: Int, reader: Reader, length: Long) = ???

  override def updateClob(columnLabel: String, reader: Reader, length: Long) = ???

  override def updateNClob(columnIndex: Int, reader: Reader, length: Long) = ???

  override def updateNClob(columnLabel: String, reader: Reader, length: Long) = ???

  override def updateNCharacterStream(columnIndex: Int, x: Reader) = ???

  override def updateNCharacterStream(columnLabel: String, reader: Reader) = ???

  override def updateAsciiStream(columnIndex: Int, x: InputStream) = ???

  override def updateBinaryStream(columnIndex: Int, x: InputStream) = ???

  override def updateCharacterStream(columnIndex: Int, x: Reader) = ???

  override def updateAsciiStream(columnLabel: String, x: InputStream) = ???

  override def updateBinaryStream(columnLabel: String, x: InputStream) = ???

  override def updateCharacterStream(columnLabel: String, reader: Reader) = ???

  override def updateBlob(columnIndex: Int, inputStream: InputStream) = ???

  override def updateBlob(columnLabel: String, inputStream: InputStream) = ???

  override def updateClob(columnIndex: Int, reader: Reader) = ???

  override def updateClob(columnLabel: String, reader: Reader) = ???

  override def updateNClob(columnIndex: Int, reader: Reader) = ???

  override def updateNClob(columnLabel: String, reader: Reader) = ???

  override def getObject[T](columnIndex: Int, `type`: Class[T]) = ???

  override def getObject[T](columnLabel: String, `type`: Class[T]) = ???

}
