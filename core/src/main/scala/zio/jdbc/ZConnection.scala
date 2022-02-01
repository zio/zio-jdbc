package zio.jdbc

import zio._
import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.Blob

/**
 * A `ZConnection` is a straightforward wrapper around `java.sql.Connection`. In order
 * to avoid needless duplication of code, one can safely access the underlying JDBC
 * `Connection` through the `access` method. Any such access will be attempted on the
 * blocking thread pool.
 */
final class ZConnection(private[jdbc] val connection: Connection) extends AnyVal {
  def access[A](f: Connection => A): ZIO[Any, Throwable, A] = ZIO.attemptBlocking(f(connection))

  private[jdbc] def executeSqlWith[A](sql: Sql[_])(f: PreparedStatement => A): ZIO[Any, Throwable, A] = access {
    connection =>
      import Sql.Segment._

      import sql.segments

      val stringBuilder = new StringBuilder()

      var i = 0

      while (i < segments.length) {
        segments(i) match {
          case Syntax(value) => stringBuilder.append(value)
          case _             =>
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
              case v                        => statement.setString(paramIndex, v.toString())
            }

            paramIndex += 1

          case Syntax(_) =>
        }
        i += 1
      }

      f(statement)
  }
}
object ZConnection {
  def apply(connection: Connection): ZConnection = new ZConnection(connection)
}
