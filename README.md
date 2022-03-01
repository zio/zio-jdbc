# ZIO JDBC

| Project Stage | CI                                       | Release                                                               |  Issues                                                     | Discord                                   |
| --- |------------------------------------------|-----------------------------------------------------------------------|--------------------------------------------------------------|-------------------------------------------|
| [![Project stage][Stage]][Stage-Page] | ![CI][badge-CI] | [![Release Artifacts][badge-sonatype-releases]][link-sonatype-releases] | [![Is it maintained?][badge-maintenance]][link-maintenance] | [![Discord][badge-discord]][link-discord] |

_ZIO JDBC_ is a small, idiomatic ZIO interface to JDBC, providing a pleasant and developer-friendly experience to low-level JDBC access.

- Idiomatic ZIO 2.0 interface to JDBC
- Secure, with protection against SQL-injection
- Fully integrated with core libraries including _ZIO Schema_, _ZIO Config_, _ZIO Logging_

## Writing Queries

`Basic.scala` (see `zio.jdbc.examples` in project)
```scala
  val age = 42

val ex0: SqlFragment = sql"create table if not exists users(name varchar(255), age int)"

// Creating SQL statements using interpolation:
val ex1: SqlFragment = sql"select * from users where age = $age"

// Selecting into tuples:
val ex2: Sql[(String, Int)] = sql"select name, age from users".as[(String, Int)]

// Inserting from tuples:
val ex3: SqlFragment = sql"insert into users (name, age)".values(("John", 42))

// dropping table
val ex4: SqlFragment = sql"drop table if exists users"
```


## Executing Statements

```scala
val res1: ZIO[ZConnectionPool, Throwable, Option[(String, Int)]] = 
  transaction {
    selectOne(sql"select name, age from users where name = 'Sherlock Holmes'".as[(String, Int)])
  }
```

## Creating a Connection Pool

```scala

 val createZIOPoolConfig: ULayer[ZConnectionPoolConfig] =
    ZLayer.succeed(ZConnectionPoolConfig.default)
 
  val properties = Map(
    "user"     -> "mysql",
    "password" -> "mysql"
  )
 
  val connectionPool: ZLayer[Clock & ZConnectionPoolConfig, Throwable, ZConnectionPool] =
    ZConnectionPool.mysql("localhost", 3306, "mysql", properties)
 
  val live: ZLayer[Clock & ZConnectionPoolConfig, Throwable, ZConnectionPool] = createZIOPoolConfig >>> connectionPool
```


## Full Example

`App.scala` (see `zio.jdbc.examples` in project)
```scala

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
    
    // One can derive a jdbc encoder from a zio-schema 
    implicit val jdbcEncoder: JdbcEncoder[User] = JdbcEncoder.fromSchema
    
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
  
  val createZIOPoolConfig: ULayer[ZConnectionPoolConfig] =
    ZLayer.succeed(ZConnectionPoolConfig.default)
  
  val properties = Map(
    "user"     -> "postgres",
    "password" -> "postgres"
  )
  
  /**
   * Pre defined ZConnection Pools exist for:
   *  Postgres, SQL Server, Oracle, MySQL and h2
   *  custom pools, can also be constructed
   */
  val connectionPool: ZLayer[Clock & ZConnectionPoolConfig, Throwable, ZConnectionPool] =
    ZConnectionPool.postgres("localhost", 5432, "postgres", properties)
  
  val program: ZIO[ZConnectionPool, Throwable, Chunk[User]] = for {
    _   <- create *> insertRow
    res <- select
    _   <- drop
  } yield res
  
  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    for {
      results <- program.provideLayer(createZIOPoolConfig >>> connectionPool)
      _       <- Console.printLine(results.mkString("\n"))
    } yield ()
}

```


To learn more about _ZIO JDBC_, check out the following references:

- [Homepage](https://zio.github.io/zio-jdbc/)
- [Contributor's guide](./.github/CONTRIBUTING.md)
- [License](LICENSE)
- [Issues](https://github.com/zio/zio-jdbc/issues)
- [Pull Requests](https://github.com/zio/zio-jdbc/pulls)

[badge-sonatype-releases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-jdbc_2.12.svg "Sonatype Releases"
[badge-CI]: https://github.com/zio/zio-jdbc/workflows/CI/badge.svg
[badge-discord]: https://img.shields.io/discord/629491597070827530?logo=discord
[badge-maintenance]: http://isitmaintained.com/badge/resolution/zio/zio-jdbc.svg
[link-sonatype-releases]: https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-jdbc_2.12/ "Sonatype Releases"
[link-discord]: https://discord.gg/2ccFBr4
[link-maintenance]: http://isitmaintained.com/project/zio/zio-jdbc
[link-zio]: https://zio.dev
[Stage]: https://img.shields.io/badge/Project%20Stage-Research-red.svg
[Stage-Page]: https://github.com/zio/zio/wiki/Project-Stages
