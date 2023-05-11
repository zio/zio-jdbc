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
final class ZConnection(private[jdbc] val underlying: Connection, state: ZConnection.State) {

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
    ZIO.succeed {
      underlying.setReadOnly(state.readOnly)
      underlying.setAutoCommit(state.autoCommit)
      underlying.setTransactionIsolation(state.transactionIsolation)
      underlying.setCatalog(state.catalog)
      underlying.setSchema(state.schema)
      underlying.setClientInfo(state.clientInfo)
    }
}

object ZConnection {

  def make(underlying: Connection): Task[ZConnection] =
    for {
      state <- State.make(underlying)
    } yield new ZConnection(underlying, state)

  private final case class State(
    autoCommit: Boolean,
    catalog: String,
    clientInfo: java.util.Properties,
    readOnly: Boolean,
    schema: String,
    transactionIsolation: Int
  )

  private object State {
    def make(connection: Connection): Task[State] =
      ZIO.attempt {
        State(
          autoCommit = connection.getAutoCommit(),
          catalog = connection.getCatalog(),
          clientInfo = connection.getClientInfo(),
          readOnly = connection.isReadOnly(),
          schema = connection.getSchema(),
          transactionIsolation = connection.getTransactionIsolation()
        )
      }
  }
}
