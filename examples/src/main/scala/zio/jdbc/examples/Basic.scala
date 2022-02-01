package zio.jdbc.examples

import zio._
import zio.jdbc._

object Basic {
  val age = 42

  // Creating SQL statements using interpolation:
  val ex1 = sql"select * from users where age = $age"

  // Selecting into tuples:
  val ex2 = sql"select name, age from users".as[(String, Int)]

  // Inserting from tuples:
  val ex3 = sql"insert into users ('name', 'age')".values(("John", 42))

  // Executing statements:
  val res1: ZIO[ZConnectionPool, Throwable, Option[(String, Int)]] =
    transaction {
      selectOne(sql"select name, age from users where name = 'Sherlock Holmes'".as[(String, Int)])
    }
}
