package zio.jdbc.examples

import zio._
import zio.jdbc._

object Basic {
  val age = 42

  val ex0: Sql[ZResultSet] = sql"create table if not exists users(name varchar(255), age int)"

  // Creating SQL statements using interpolation:
  val ex1: Sql[ZResultSet] = sql"select * from users where age = $age"

  // Selecting into tuples:
  val ex2: Sql[(String, Int)] = sql"select name, age from users".as[(String, Int)]

  // Inserting from tuples:
  val ex3: Sql[ZResultSet] = sql"insert into users (name, age)".values(("John", 42))

  // Composing requests:
  val keyColumn                                = "key"
  val valueColumn                              = "value"
  def ex4(offset: Long): Sql[(String, String)] =
    (s"select $keyColumn, $valueColumn from events" ++ sql"where id > $offset").as[(String, String)]

  // Executing statements:
  val res1: ZIO[ZConnectionPool, Throwable, Option[(String, Int)]] =
    transaction {
      selectOne(sql"select name, age from users where name = 'Sherlock Holmes'".as[(String, Int)])
    }
}
