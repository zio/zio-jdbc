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

import java.sql.{ Connection, PreparedStatement }

/**
 * A `ZConnection` is a straightforward wrapper around `java.sql.Connection`. In order
 * to avoid needless duplication of code, one can safely access the underlying JDBC
 * `Connection` through the `access` method. Any such access will be attempted on the
 * blocking thread pool.
 */
final class ZConnection(private[jdbc] val connection: Connection) extends AnyVal {
  def access[A](f: Connection => A): ZIO[Any, Throwable, A] = ZIO.attemptBlocking(f(connection))

  def accessZIO[A](f: Connection => ZIO[Scope, Throwable, A]): ZIO[Scope, Throwable, A] = ZIO.blocking(f(connection))

  def close: Task[Any]    = access(_.close())
  def rollback: Task[Any] = access(_.rollback())

  private[jdbc] def executeSqlWith[A](
    sql: Sql[_]
  )(f: PreparedStatement => ZIO[Scope, Throwable, A]): ZIO[Scope, Throwable, A] =
    accessZIO { connection =>
      for {
        statement <- ZIO.acquireRelease(ZIO.attempt {
                       val sb = new StringBuilder()
                       sql.foreachSegment(syntax => sb.append(syntax.value))(_ => sb.append("?"))
                       connection.prepareStatement(sb.toString)
                     })(statement => ZIO.attemptBlocking(statement.close()).ignoreLogged)
        _         <- ZIO.attempt {
                       var paramIndex = 1
                       sql.foreachSegment(_ => ()) { param =>
                         param.setter(statement, paramIndex, param.value)
                         paramIndex += 1

                       }
                     }
        result    <- f(statement)
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
      closed    <- ZIO.attempt(this.connection.isClosed)
      statement <- ZIO.attempt(this.connection.prepareStatement("SELECT 1"))
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
    ZIO.attempt(this.connection.isValid(timeout))
}

object ZConnection {
  def apply(connection: Connection): ZConnection = new ZConnection(connection)
}
