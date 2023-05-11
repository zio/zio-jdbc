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

    def restore(): Unit = {
      underlying.setAutoCommit(initialAutoCommit)
      underlying.setCatalog(initialCatalog)
      underlying.setClientInfo(initialClientInfo)
      underlying.setReadOnly(initialReadOnly)
      underlying.setSchema(initialSchema)
      underlying.setTransactionIsolation(initialTransactionIsolation)
    }

    def abort(executor: java.util.concurrent.Executor): Unit                                    =
      underlying.abort(executor)
    def clearWarnings(): Unit                                                                   =
      underlying.clearWarnings()
    def close(): Unit                                                                           =
      underlying.close()
    def commit(): Unit                                                                          =
      underlying.commit()
    def createArrayOf(x$1: String, x$2: Array[Object]): java.sql.Array                          =
      underlying.createArrayOf(x$1, x$2)
    def createBlob(): java.sql.Blob                                                             =
      underlying.createBlob()
    def createClob(): java.sql.Clob                                                             =
      underlying.createClob()
    def createNClob(): java.sql.NClob                                                           =
      underlying.createNClob()
    def createSQLXML(): java.sql.SQLXML                                                         =
      underlying.createSQLXML()
    def createStatement(x$1: Int, x$2: Int, x$3: Int): java.sql.Statement                       =
      underlying.createStatement(x$1, x$2, x$3)
    def createStatement(x$1: Int, x$2: Int): java.sql.Statement                                 =
      underlying.createStatement(x$1, x$2)
    def createStatement(): java.sql.Statement                                                   =
      underlying.createStatement()
    def createStruct(x$1: String, x$2: Array[Object]): java.sql.Struct                          =
      underlying.createStruct(x$1, x$2)
    def getAutoCommit(): Boolean                                                                =
      underlying.getAutoCommit()
    def getCatalog(): String                                                                    =
      underlying.getCatalog()
    def getClientInfo(): java.util.Properties                                                   =
      underlying.getClientInfo()
    def getClientInfo(x$1: String): String                                                      =
      underlying.getClientInfo(x$1)
    def getHoldability(): Int                                                                   =
      underlying.getHoldability()
    def getMetaData(): java.sql.DatabaseMetaData                                                =
      underlying.getMetaData()
    def getNetworkTimeout(): Int                                                                =
      underlying.getNetworkTimeout()
    def getSchema(): String                                                                     =
      underlying.getSchema()
    def getTransactionIsolation(): Int                                                          =
      underlying.getTransactionIsolation()
    def getTypeMap(): java.util.Map[String, Class[_]]                                           =
      underlying.getTypeMap()
    def getWarnings(): java.sql.SQLWarning                                                      =
      underlying.getWarnings()
    def isClosed(): Boolean                                                                     =
      underlying.isClosed()
    def isReadOnly(): Boolean                                                                   =
      underlying.isReadOnly()
    def isValid(x$1: Int): Boolean                                                              =
      underlying.isValid(x$1)
    def nativeSQL(x$1: String): String                                                          =
      underlying.nativeSQL(x$1)
    def prepareCall(x$1: String, x$2: Int, x$3: Int, x$4: Int): java.sql.CallableStatement      =
      underlying.prepareCall(x$1, x$2, x$3, x$4)
    def prepareCall(x$1: String, x$2: Int, x$3: Int): java.sql.CallableStatement                =
      underlying.prepareCall(x$1, x$2, x$3)
    def prepareCall(x$1: String): java.sql.CallableStatement                                    =
      underlying.prepareCall(x$1)
    def prepareStatement(x$1: String, x$2: Array[String]): java.sql.PreparedStatement           =
      underlying.prepareStatement(x$1, x$2)
    def prepareStatement(x$1: String, x$2: Array[Int]): java.sql.PreparedStatement              =
      underlying.prepareStatement(x$1, x$2)
    def prepareStatement(x$1: String, x$2: Int): java.sql.PreparedStatement                     =
      underlying.prepareStatement(x$1, x$2)
    def prepareStatement(x$1: String, x$2: Int, x$3: Int, x$4: Int): java.sql.PreparedStatement =
      underlying.prepareStatement(x$1, x$2, x$3, x$4)
    def prepareStatement(x$1: String, x$2: Int, x$3: Int): java.sql.PreparedStatement           =
      underlying.prepareStatement(x$1, x$2, x$3)
    def prepareStatement(x$1: String): java.sql.PreparedStatement                               =
      underlying.prepareStatement(x$1)
    def releaseSavepoint(x$1: java.sql.Savepoint): Unit                                         =
      underlying.releaseSavepoint(x$1)
    def rollback(x$1: java.sql.Savepoint): Unit                                                 =
      underlying.rollback(x$1)
    def rollback(): Unit                                                                        =
      underlying.rollback()
    def setAutoCommit(x$1: Boolean): Unit                                                       =
      underlying.setAutoCommit(x$1)
    def setCatalog(x$1: String): Unit                                                           =
      underlying.setCatalog(x$1)
    def setClientInfo(x$1: java.util.Properties): Unit                                          =
      underlying.setClientInfo(x$1)
    def setClientInfo(x$1: String, x$2: String): Unit                                           =
      underlying.setClientInfo(x$1, x$2)
    def setHoldability(x$1: Int): Unit                                                          =
      underlying.setHoldability(x$1)
    def setNetworkTimeout(x$1: java.util.concurrent.Executor, x$2: Int): Unit                   =
      underlying.setNetworkTimeout(x$1, x$2)
    def setReadOnly(x$1: Boolean): Unit                                                         =
      underlying.setReadOnly(x$1)
    def setSavepoint(x$1: String): java.sql.Savepoint                                           =
      underlying.setSavepoint(x$1)
    def setSavepoint(): java.sql.Savepoint                                                      =
      underlying.setSavepoint()
    def setSchema(x$1: String): Unit                                                            =
      underlying.setSchema(x$1)
    def setTransactionIsolation(x$1: Int): Unit                                                 =
      underlying.setTransactionIsolation(x$1)
    def setTypeMap(x$1: java.util.Map[String, Class[_]]): Unit                                  =
      underlying.setTypeMap(x$1)
    def isWrapperFor(x$1: Class[_]): Boolean                                                    =
      underlying.isWrapperFor(x$1)
    def unwrap[T](x$1: Class[T]): T                                                             =
      underlying.unwrap(x$1)
  }
}
