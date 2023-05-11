/*
 * Copyright 2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package zio.jdbc

import zio._

import java.sql.{ Connection, PreparedStatement, Statement }

/**
 * A `ZConnection` is a straightforward wrapper around `java.sql.Connection`. In order
 * to avoid needless duplication of code, one can safely access the underlying JDBC
 * `Connection` through the `access` method. Any such access will be attempted on the
 * blocking thread pool.
 */
final class ZConnection(private[jdbc] val underlying: ZConnection.Restorable) extends AnyVal {

  def access[A](f: Connection => A): ZIO[Any, Throwable, A] =
    ZIO.attemptBlocking(f(underlying))

  def accessZIO[A](f: Connection => ZIO[Scope, Throwable, A]): ZIO[Scope, Throwable, A] =
    ZIO.blocking(f(underlying))

  def close: Task[Any]    = access(_.close())
  def rollback: Task[Any] = access(_.rollback())

  private[jdbc] def executeSqlWith[A](
    sql: SqlFragment
  )(f: PreparedStatement => ZIO[Scope, Throwable, A]): ZIO[Scope, Throwable, A] =
    accessZIO { connection =>
      for {
        transactionIsolationLevel <- currentTransactionIsolationLevel.get
        statement                 <- ZIO.acquireRelease(ZIO.attempt {
                                       val sb = new StringBuilder()
                                       sql.foreachSegment(syntax => sb.append(syntax.value))(_ => sb.append("?"))
                                       transactionIsolationLevel.foreach { transactionIsolationLevel =>
                                         connection.setTransactionIsolation(transactionIsolationLevel.toInt)
                                       }
                                       connection.prepareStatement(sb.toString, Statement.RETURN_GENERATED_KEYS)
                                     })(statement => ZIO.attemptBlocking(statement.close()).ignoreLogged)
        _                         <- ZIO.attempt {
                                       var paramIndex = 1
                                       sql.foreachSegment(_ => ()) { param =>
                                         param.setter.setValue(statement, paramIndex, param.value)
                                         paramIndex += 1
                                       }
                                     }
        result                    <- f(statement)
      } yield result
    }.tapErrorCause { cause => // TODO: Question: do we want logging here, switch to debug for now
      ZIO.logAnnotate("SQL", sql.toString)(
        ZIO.logDebugCause(s"Error executing SQL due to: ${cause.prettyPrint}", cause)
      )
    }

  /**
   * Return whether the connection is still alive or not,
   * trying to prepare a statement and managing the exception SQLException
   * if the connection can not do it.
   *
   * see: https://www.baeldung.com/jdbc-connection-status
   *
   * @param zc the connection to look into
   * @return true if the connection is alive (valid), false otherwise
   */
  def isValid(): Task[Boolean] =
    for {
      closed    <- ZIO.attempt(this.underlying.isClosed)
      statement <- ZIO.attempt(this.underlying.prepareStatement("SELECT 1"))
      isAlive   <- ZIO.succeed(!closed && statement != null)
    } yield isAlive

  /**
   * Returns whether the connection is still alive or not, providing a timeout,
   * using the isValid(timeout) method on the java.sql.Connection interface
   *
   * see: https://www.baeldung.com/jdbc-connection-status
   *
   * @param zc the connection to look into
   * @return true if the connection is alive (valid), false otherwise
   */
  def isValid(timeout: Int): Task[Boolean] =
    ZIO.attempt(this.underlying.isValid(timeout))

  private[jdbc] def restore: UIO[Unit] =
    ZIO.succeed(underlying.restore())
}

object ZConnection {

  def make(underlying: Connection): Task[ZConnection] =
    for {
      restorable <- ZIO.attempt(new Restorable(underlying))
    } yield new ZConnection(restorable)

  private[jdbc] class Restorable(underlying: Connection) extends Connection {
    private[this] val initialAutoCommit           = underlying.getAutoCommit()
    private[this] val initialCatalog              = underlying.getCatalog()
    private[this] val initialClientInfo           = underlying.getClientInfo()
    private[this] val initialReadOnly             = underlying.isReadOnly()
    private[this] val initialSchema               = underlying.getSchema()
    private[this] val initialTransactionIsolation = underlying.getTransactionIsolation()

    def abort(executor: java.util.concurrent.Executor): Unit                                                          =
      underlying.abort(executor)
    def clearWarnings(): Unit                                                                                         =
      underlying.clearWarnings()
    def close(): Unit                                                                                                 =
      underlying.close()
    def commit(): Unit                                                                                                =
      underlying.commit()
    def createArrayOf(typeName: String, elements: Array[Object]): java.sql.Array                                      =
      underlying.createArrayOf(typeName, elements)
    def createBlob(): java.sql.Blob                                                                                   =
      underlying.createBlob()
    def createClob(): java.sql.Clob                                                                                   =
      underlying.createClob()
    def createNClob(): java.sql.NClob                                                                                 =
      underlying.createNClob()
    def createSQLXML(): java.sql.SQLXML                                                                               =
      underlying.createSQLXML()
    def createStatement(resultSetType: Int, resultSetConcurrency: Int, resultSetHoldability: Int): java.sql.Statement =
      underlying.createStatement(resultSetType, resultSetConcurrency, resultSetHoldability)
    def createStatement(resultSetType: Int, resultSetConcurrency: Int): java.sql.Statement                            =
      underlying.createStatement(resultSetType, resultSetConcurrency)
    def createStatement(): java.sql.Statement                                                                         =
      underlying.createStatement()
    def createStruct(typeName: String, attributes: Array[Object]): java.sql.Struct                                    =
      underlying.createStruct(typeName, attributes)
    def getAutoCommit(): Boolean                                                                                      =
      underlying.getAutoCommit()
    def getCatalog(): String                                                                                          =
      underlying.getCatalog()
    def getClientInfo(): java.util.Properties                                                                         =
      underlying.getClientInfo()
    def getClientInfo(name: String): String                                                                           =
      underlying.getClientInfo(name)
    def getHoldability(): Int                                                                                         =
      underlying.getHoldability()
    def getMetaData(): java.sql.DatabaseMetaData                                                                      =
      underlying.getMetaData()
    def getNetworkTimeout(): Int                                                                                      =
      underlying.getNetworkTimeout()
    def getSchema(): String                                                                                           =
      underlying.getSchema()
    def getTransactionIsolation(): Int                                                                                =
      underlying.getTransactionIsolation()
    def getTypeMap(): java.util.Map[String, Class[_]]                                                                 =
      underlying.getTypeMap()
    def getWarnings(): java.sql.SQLWarning                                                                            =
      underlying.getWarnings()
    def isClosed(): Boolean                                                                                           =
      underlying.isClosed()
    def isReadOnly(): Boolean                                                                                         =
      underlying.isReadOnly()
    def isValid(timeout: Int): Boolean                                                                                =
      underlying.isValid(timeout)
    def isWrapperFor(iface: Class[_]): Boolean                                                                        =
      underlying.isWrapperFor(iface)
    def nativeSQL(sql: String): String                                                                                =
      underlying.nativeSQL(sql)
    def prepareCall(
      sql: String,
      resultSetType: Int,
      resultSetConcurrency: Int,
      resultSetHoldability: Int
    ): java.sql.CallableStatement =
      underlying.prepareCall(sql, resultSetType, resultSetConcurrency, resultSetHoldability)
    def prepareCall(sql: String, resultSetType: Int, resultSetConcurrency: Int): java.sql.CallableStatement           =
      underlying.prepareCall(sql, resultSetType, resultSetConcurrency)
    def prepareCall(sql: String): java.sql.CallableStatement                                                          =
      underlying.prepareCall(sql)
    def prepareStatement(sql: String, columnNames: Array[String]): java.sql.PreparedStatement                         =
      underlying.prepareStatement(sql, columnNames)
    def prepareStatement(sql: String, columnIndexes: Array[Int]): java.sql.PreparedStatement                          =
      underlying.prepareStatement(sql, columnIndexes)
    def prepareStatement(sql: String, autoGeneratedKeys: Int): java.sql.PreparedStatement                             =
      underlying.prepareStatement(sql, autoGeneratedKeys)
    def prepareStatement(
      sql: String,
      resultSetType: Int,
      resultSetConcurrency: Int,
      resultSetHoldability: Int
    ): java.sql.PreparedStatement =
      underlying.prepareStatement(sql, resultSetType, resultSetConcurrency, resultSetHoldability)
    def prepareStatement(sql: String, resultSetType: Int, resultSetConcurrency: Int): java.sql.PreparedStatement      =
      underlying.prepareStatement(sql, resultSetType, resultSetConcurrency)
    def prepareStatement(sql: String): java.sql.PreparedStatement                                                     =
      underlying.prepareStatement(sql)
    def releaseSavepoint(savepoint: java.sql.Savepoint): Unit                                                         =
      underlying.releaseSavepoint(savepoint)
    def restore(): Unit                                                                                               = {
      underlying.setAutoCommit(initialAutoCommit)
      underlying.setCatalog(initialCatalog)
      underlying.setClientInfo(initialClientInfo)
      underlying.setReadOnly(initialReadOnly)
      underlying.setSchema(initialSchema)
      underlying.setTransactionIsolation(initialTransactionIsolation)
    }
    def rollback(savepoint: java.sql.Savepoint): Unit                                                                 =
      underlying.rollback(savepoint)
    def rollback(): Unit                                                                                              =
      underlying.rollback()
    def setAutoCommit(autoCommit: Boolean): Unit                                                                      =
      underlying.setAutoCommit(autoCommit)
    def setCatalog(catalog: String): Unit                                                                             =
      underlying.setCatalog(catalog)
    def setClientInfo(properties: java.util.Properties): Unit                                                         =
      underlying.setClientInfo(properties)
    def setClientInfo(name: String, value: String): Unit                                                              =
      underlying.setClientInfo(name, value)
    def setHoldability(holdability: Int): Unit                                                                        =
      underlying.setHoldability(holdability)
    def setNetworkTimeout(executor: java.util.concurrent.Executor, milliseconds: Int): Unit                           =
      underlying.setNetworkTimeout(executor, milliseconds)
    def setReadOnly(readOnly: Boolean): Unit                                                                          =
      underlying.setReadOnly(readOnly)
    def setSavepoint(name: String): java.sql.Savepoint                                                                =
      underlying.setSavepoint(name)
    def setSavepoint(): java.sql.Savepoint                                                                            =
      underlying.setSavepoint()
    def setSchema(schema: String): Unit                                                                               =
      underlying.setSchema(schema)
    def setTransactionIsolation(level: Int): Unit                                                                     =
      underlying.setTransactionIsolation(level)
    def setTypeMap(map: java.util.Map[String, Class[_]]): Unit                                                        =
      underlying.setTypeMap(map)
    def unwrap[T](iface: Class[T]): T                                                                                 =
      underlying.unwrap(iface)
  }
}
