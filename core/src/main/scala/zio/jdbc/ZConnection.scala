package zio.jdbc

import zio._
import java.sql.Connection

final class ZConnection(private[jdbc] val connection: Connection) extends AnyVal {
  def execute[A](f: Connection => A): ZIO[Any, Throwable, A] = ZIO.attemptBlocking(f(connection))
}
object ZConnection {
  def apply(connection: Connection): ZConnection = new ZConnection(connection)
}
