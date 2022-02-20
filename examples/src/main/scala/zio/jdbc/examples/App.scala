package zio.jdbc.examples

import zio._
import zio.jdbc._
import zio.schema.Schema

/**
 * You'll need the appropriate JDBC driver, and a database running.
 */
object App extends ZIOAppDefault {
  final case class User(name: String, age: Int)

  object User {
    import Schema.Field

    implicit val schema: Schema[User] =
      Schema.CaseClass2[String, Int, User](
        Field("name", Schema[String]),
        Field("age", Schema[Int]),
        (name, age) => User(name, age),
        _.name,
        _.age
      )

    // One can derive a jdbc decoder from a zio-schema or
    implicit val jdbcDecoder: JdbcDecoder[User] = JdbcDecoder.fromSchema

    // a custom decoder from a tuple
    // implicit val jdbcDecoder = JdbcDecoder[(String, Int)].map[User](t => User(t._1, t._2))
  }

  val create: ZIO[ZConnectionPool, Throwable, Unit] = transaction {
    execute(Basic.ex0)
  }

  val insertRow: ZIO[ZConnectionPool, Throwable, Long] = transaction {
    insert(Basic.ex3)
  }

  val select: ZIO[ZConnectionPool, Throwable, Chunk[User]] = transaction {
    selectAll(Basic.ex2.as[User])
  }

  val drop: ZIO[ZConnectionPool, Throwable, Unit] = transaction {
    execute(Basic.ex4)
  }

  val zioPoolConfig: ULayer[ZConnectionPoolConfig] =
    ZLayer.succeed(ZConnectionPoolConfig.default)

  val properties: Map[String, String] = Map(
    "user"     -> "mysql",
    "password" -> "mysql"
  )

  /**
   * Pre defined ZConnection Pools exist for:
   *  Postgres, SQL Server, Oracle, MySQL and h2
   *  custom pools, can also be constructed
   */
  val connectionPool: ZLayer[Clock & ZConnectionPoolConfig, Throwable, ZConnectionPool] =
    ZConnectionPool.mysql("localhost", 3306, "mysql", properties)

  val program: ZIO[ZConnectionPool, Throwable, Chunk[User]] = for {
    _   <- create *> insertRow
    res <- select
    _   <- drop
  } yield res

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    for {
      results <- program.provideLayer(zioPoolConfig >>> connectionPool)
      _       <- Console.printLine(results.mkString("\n"))
    } yield ()
}
