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

import java.sql.{ Blob, Connection, PreparedStatement }

/**
 * A `ZConnection` is a straightforward wrapper around `java.sql.Connection`. In order
 * to avoid needless duplication of code, one can safely access the underlying JDBC
 * `Connection` through the `access` method. Any such access will be attempted on the
 * blocking thread pool.
 */
final class ZConnection(private[jdbc] val connection: Connection) extends AnyVal {
  def access[A](f: Connection => A): ZIO[Any, Throwable, A] = ZIO.attemptBlocking(f(connection))

  private[jdbc] def executeSqlWith[A](sql: Sql[_])(f: PreparedStatement => A): ZIO[Any, Throwable, A] =
    access { connection =>
      import Sql.Segment._

      val segments = sql.segments

      val stringBuilder = new StringBuilder()

      var i = 0

      while (i < segments.length) {
        segments(i) match {
          case Syntax(value) => stringBuilder.append(value)
          case _             => stringBuilder.append("?")
        }
        i += 1
      }

      val statement = connection.prepareStatement(stringBuilder.toString)

      i = 0
      var paramIndex = 1

      while (i < segments.length) {
        segments(i) match {
          case Param(value) =>
            // TODO: Support more types here
            value match {
              case v: String                => statement.setString(paramIndex, v)
              case v: Int                   => statement.setInt(paramIndex, v)
              case v: Long                  => statement.setLong(paramIndex, v)
              case v: Short                 => statement.setShort(paramIndex, v)
              case v: Byte                  => statement.setByte(paramIndex, v)
              case v: Char                  => statement.setString(paramIndex, v.toString)
              case v: Double                => statement.setDouble(paramIndex, v)
              case v: Blob                  => statement.setBlob(paramIndex, v)
              case v: java.math.BigDecimal  => statement.setBigDecimal(paramIndex, v)
              case v: scala.math.BigDecimal => statement.setBigDecimal(paramIndex, v.bigDecimal)
              case v: java.math.BigInteger  => statement.setBigDecimal(paramIndex, new java.math.BigDecimal(v))
              case v: scala.math.BigInt     => statement.setBigDecimal(paramIndex, new java.math.BigDecimal(v.bigInteger))
              case v: java.sql.Date         => statement.setDate(paramIndex, v)
              case v: java.sql.Time         => statement.setTime(paramIndex, v)
              case v: java.sql.Timestamp    => statement.setTimestamp(paramIndex, v)
              case v: Boolean               => statement.setBoolean(paramIndex, v)
              case v: Float                 => statement.setFloat(paramIndex, v)
              case v                        => statement.setString(paramIndex, v.toString)
            }

            paramIndex += 1

          case Syntax(_) =>
        }
        i += 1
      }

      f(statement)
    }.tapErrorCause { cause =>
      ZIO.logAnnotate("SQL", sql.toString)(ZIO.logError(s"Error executing SQL due to: ${cause.prettyPrint}"))
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
  def isValid(): Task[Boolean] = {
    for {
      closed <- ZIO.attempt(this.connection.isClosed)
      statement <- ZIO.attempt(this.connection.prepareStatement("SELECT 1"))
      isAlive <- ZIO.succeed(!closed && statement != null)
    } yield isAlive
  }

  /**
   * Returns whether the connection is still alive or not, providing a timeout,
   * using the isValid(timeout) method on the java.sql.Connection interface
   *
   * see: https://www.baeldung.com/jdbc-connection-status
   *
   * @param zc the connection to look into
   * @return true if the connection is alive (valid), false otherwise
   */
  def isValid(timeout: Int): Task[Boolean] = {
    ZIO.attempt(this.connection.isValid(timeout))
  }
}
object ZConnection {
  def apply(connection: Connection): ZConnection = new ZConnection(connection)
}
