package zio

import zio.stream._

import scala.language.implicitConversions

package object jdbc {
  implicit def sqlInterpolator(sc: StringContext): SqlInterpolator = new SqlInterpolator(sc)

  def delete(sql: SqlStatement[ZResultSet]): ZIO[ZConnection, Throwable, Long] =
    for {
      connection <- ZIO.service[ZConnection]
      result     <- connection.execute(conn => sql.toStatement(conn).executeLargeUpdate())
    } yield result

  def execute(sql: SqlStatement[ZResultSet]): ZIO[ZConnection, Throwable, Unit] =
    for {
      connection <- ZIO.service[ZConnection]
      _          <- connection.execute(conn => sql.toStatement(conn).executeQuery())
    } yield ()

  def selectAll[A](sql: SqlStatement[A]): ZIO[ZConnection, Throwable, Chunk[A]] =
    for {
      connection <- ZIO.service[ZConnection]
      result     <- connection.execute(conn => sql.toStatement(conn).executeQuery())
      chunk      <- Task {
                      val builder = ChunkBuilder.make[A]()
                      val zrs     = ZResultSet(result)
                      while (result.next())
                        builder += sql.decode(zrs)
                      builder.result()
                    }
    } yield chunk

  def selectOne[A](sql: SqlStatement[A]): ZIO[ZConnection, Throwable, Option[A]] =
    for {
      connection <- ZIO.service[ZConnection]
      result     <- connection.execute(conn => sql.toStatement(conn).executeQuery())
      option     <- if (result.next()) ZIO.succeed(None) else ZIO.some(sql.decode(ZResultSet(result)))
    } yield option

  def selectStream[A](sql: SqlStatement[A]): ZStream[ZConnection, Throwable, A] =
    ZStream.unwrap {
      for {
        connection <- ZIO.service[ZConnection]
        result     <- connection.execute(conn => sql.toStatement(conn).executeQuery())
        zrs         = ZResultSet(result)
        stream      = ZStream.fromZIOOption(Task.suspend {
                        if (result.next()) ZIO.attempt(Some(sql.decode(zrs))) else ZIO.succeed(None)
                      }.some)
      } yield stream
    }

  def update(sql: SqlStatement[ZResultSet]): ZIO[ZConnection, Throwable, Long] =
    for {
      connection <- ZIO.service[ZConnection]
      result     <- connection.execute(conn => sql.toStatement(conn).executeLargeUpdate())
    } yield result
}
