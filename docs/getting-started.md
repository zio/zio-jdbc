---
id: getting-started
title: "Getting Started with ZIO JDBC"
sidebar_label: "Getting Started"
---

## Writing Queries

`Basic.scala` (see `zio.jdbc.examples` in project)

```scala
  val age = 42

val ex0: SqlFragment = sql"create table if not exists users(name varchar(255), age int)"

// Creating SQL statements using interpolation:
val ex1: SqlFragment = sql"select * from users where age = $age"

// Selecting into tuples:
val ex2: Query[(String, Int)] = sql"select name, age from users".query[(String, Int)]

// Inserting from tuples:
val ex3: SqlFragment = sql"insert into users (name, age)".values(("John", 42))

// dropping table
val ex4: SqlFragment = sql"drop table if exists users"
```


## Executing Statements

```scala
val res1: ZIO[ZConnectionPool, Throwable, Option[(String, Int)]] = 
  transaction {
    sql"select name, age from users where name = 'Sherlock Holmes'".query[(String, Int)].selectOne
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
 
  val connectionPool: ZLayer[ZConnectionPoolConfig, Throwable, ZConnectionPool] =
    ZConnectionPool.mysql("localhost", 3306, "mysql", properties)
 
  val live: ZLayer[ZConnectionPoolConfig, Throwable, ZConnectionPool] = createZIOPoolConfig >>> connectionPool
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
    Basic.ex0.execute
  }
  
  val insertRow: ZIO[ZConnectionPool, Throwable, Long] = transaction {
    Basic.ex3.insert
  }
  
  val select: ZIO[ZConnectionPool, Throwable, Chunk[User]] = transaction {
    Basic.ex2.as[User].selectAll
  }
  
  val drop: ZIO[ZConnectionPool, Throwable, Unit] = transaction {
    Basic.ex4.execute
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
  val connectionPool: ZLayer[ZConnectionPoolConfig, Throwable, ZConnectionPool] =
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
