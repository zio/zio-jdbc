package zio.jdbc.examples

import zio._
import zio.jdbc._
import zio.schema.{ Schema, TypeId }

/**
 * You'll need the appropriate JDBC driver, and a database running.
 */
object App extends ZIOAppDefault {
  import zio.jdbc.examples.User._

  val create: ZIO[ZConnectionPool, Throwable, Unit] = transaction {
    Basic.ex0.execute
  }

  val insertRow: ZIO[ZConnectionPool, Throwable, UpdateResult[Long]] = transaction {
    sql"insert into users (name, age)".values(sampleUser1, sampleUser2).insertWithKeys
  }

  val select: ZIO[ZConnectionPool, Throwable, Chunk[User]] = transaction {
    Basic.ex2.as[User].selectAll
  }

  val drop: ZIO[ZConnectionPool, Throwable, Unit] = transaction {
    Basic.ex5.execute
  }

  val zioPoolConfig: ULayer[ZConnectionPoolConfig] =
    ZLayer.succeed(ZConnectionPoolConfig.default)

  val properties: Map[String, String] = Map(
    "user"     -> "postgres",
    "password" -> "postgres"
  )

  /**
   * Pre defined ZConnection Pools exist for:
   *  Postgres, SQL Server, Oracle, MySQL and h2
   *  custom pools, can also be constructed
   */
  val connectionPool: ZLayer[ZConnectionPoolConfig, Throwable, ZConnectionPool] =
    ZConnectionPool.postgres("localhost", 5432, "postgres", properties)

  val program: ZIO[ZConnectionPool, Throwable, Chunk[User]] = for {
    _   <- create *> insertRow
    res <- select
    _   <- drop
  } yield res

  override def run: ZIO[Scope with ZIOAppArgs, Any, Any] =
    for {
      results <- program.provideLayer(zioPoolConfig >>> connectionPool)
      _       <- Console.printLine(results.mkString("\n"))
    } yield ()
}

final case class User(name: String, age: Int)

object User {
  import Schema.Field

  implicit val schema: Schema[User] =
    Schema.CaseClass2[String, Int, User](
      TypeId.parse(classOf[User].getName),
      Field("name", Schema[String], get0 = _.name, set0 = (x, v) => x.copy(name = v)),
      Field("age", Schema[Int], get0 = _.age, set0 = (x, v) => x.copy(age = v)),
      User.apply
    )

  // One can derive a jdbc codec from a zio-schema or
  implicit val jdbcDecoder: JdbcDecoder[User] = JdbcDecoder.fromSchema
  implicit val jdbcEncoder: JdbcEncoder[User] = JdbcEncoder.fromSchema

  val sampleUser1: User = User("John", 42)
  val sampleUser2: User = User("Sandra", 27)

  // a custom decoder from a tuple
  // implicit val jdbcDecoder = JdbcDecoder[(String, Int)].map[User](t => User(t._1, t._2))
}
