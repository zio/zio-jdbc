package zio

import zio.stream._

import scala.language.implicitConversions

package object jdbc {
  implicit def sqlInterpolator(sc: StringContext): SqlInterpolator = new SqlInterpolator(sc)

  /**
   * Executes a SQL delete query.
   */
  def delete(sql: Sql[ZResultSet]): ZIO[ZConnection, Throwable, Long] =
    for {
      connection <- ZIO.service[ZConnection]
      result     <- connection.executeSqlWith(sql)(_.executeLargeUpdate())
    } yield result

  /**
   * Executes a SQL query, such as one that creates a table.
   */
  def execute(sql: Sql[ZResultSet]): ZIO[ZConnection, Throwable, Unit] =
    for {
      connection <- ZIO.service[ZConnection]
      _          <- connection.executeSqlWith(sql)(_.executeQuery())
    } yield ()

  /**
   * Performs a SQL select query, returning all results in a chunk.
   */
  def selectAll[A](sql: Sql[A]): ZIO[ZConnection, Throwable, Chunk[A]] =
    for {
      connection <- ZIO.service[ZConnection]
      result     <- connection.executeSqlWith(sql)(_.executeQuery())
      chunk      <- Task {
                      val builder = ChunkBuilder.make[A]()
                      val zrs     = ZResultSet(result)
                      while (result.next())
                        builder += sql.decode(zrs)
                      builder.result()
                    }
    } yield chunk

  /**
   * Performs a SQL select query, returning the first result, if any.
   */
  def selectOne[A](sql: Sql[A]): ZIO[ZConnection, Throwable, Option[A]] =
    for {
      connection <- ZIO.service[ZConnection]
      result     <- connection.executeSqlWith(sql)(_.executeQuery())
      option     <- if (result.next()) ZIO.none else ZIO.some(sql.decode(ZResultSet(result)))
    } yield option

  /**
   * Performs a SQL select query, returning a stream of results.
   */
  def selectStream[A](sql: Sql[A]): ZStream[ZConnection, Throwable, A] =
    ZStream.unwrap {
      for {
        connection <- ZIO.service[ZConnection]
        result     <- connection.executeSqlWith(sql)(_.executeQuery())
        zrs         = ZResultSet(result)
        stream      = ZStream.fromZIOOption(Task.suspend {
                        if (result.next()) ZIO.attempt(Some(sql.decode(zrs))) else ZIO.none
                      }.some)
      } yield stream
    }

  /**
   * A new transaction, which may be applied to ZIO effects that require a
   * connection in order to execute such effects in the transaction.
   */
  val transaction: ZLayer[ZConnectionPool, Throwable, ZConnection] =
    ZLayer(ZIO.serviceWith[ZConnectionPool](_.transaction)).flatten

  /**
   * Performs a SQL update query, returning a count of rows updated.
   */
  def update(sql: Sql[ZResultSet]): ZIO[ZConnection, Throwable, Long] =
    for {
      connection <- ZIO.service[ZConnection]
      result     <- connection.executeSqlWith(sql)(_.executeLargeUpdate())
    } yield result
}
