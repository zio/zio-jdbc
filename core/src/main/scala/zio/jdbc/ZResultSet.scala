package zio.jdbc

import java.sql.ResultSet

import zio._

/**
 * A `ZResultSet` is a straightforward wrapper around `java.sql.ResultSet`. In order
 * to avoid needless duplication of code, one can safely access the underlying JDBC
 * `ResultSet` through the `access` method. Any such access will be attempted on the
 * blocking thread pool.
 */
final class ZResultSet(private[jdbc] val resultSet: ResultSet) extends AnyVal {
  def access[A](f: ResultSet => A): ZIO[Any, Throwable, A] = ZIO.attemptBlocking(f(resultSet))
}
object ZResultSet {
  def apply(resultSet: ResultSet): ZResultSet = new ZResultSet(resultSet)
}
