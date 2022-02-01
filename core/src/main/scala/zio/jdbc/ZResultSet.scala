package zio.jdbc

import java.sql.ResultSet

import zio._

final class ZResultSet(private[jdbc] val resultSet: ResultSet) extends AnyVal {
  final def execute[A](f: ResultSet => A): ZIO[Any, Throwable, A] = ZIO.attemptBlocking(f(resultSet))
}
object ZResultSet {
  def apply(resultSet: ResultSet): ZResultSet = new ZResultSet(resultSet)
}
