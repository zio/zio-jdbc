package zio.jdbc

import zio._
import zio.test.TestAspect._
import zio.test._
import zio.schema._

object ZConnectionPoolSpec extends ZIOSpecDefault {
  final case class Person(name: String, age: Int)

  object Person {
    import Schema.Field

    implicit val schema: Schema[Person] =
      Schema.CaseClass2[String, Int, Person](
        Field("name", Schema[String]),
        Field("age", Schema[Int]),
        (name, age) => Person(name, age),
        _.name,
        _.age
      )
  }
  val sherlockHolmes = User("Sherlock Holmes", 42)

  val createUsers: ZIO[ZConnectionPool with Any, Throwable, Unit] =
    transaction {
      execute(sql"""
      create table users (
        id identity primary key,
        name varchar not null,
        age int not null
      )
      """)
    }

  val insertSherlock =
    transaction {
      insert {
        sql"insert into users values (default, ${sherlockHolmes.name}, ${sherlockHolmes.age})"
      }
    }

  final case class User(name: String, age: Int)
  object User {
    implicit val jdbcDecoder: JdbcDecoder[User] =
      JdbcDecoder[(String, Int)].map[User](t => User(t._1, t._2))
  }

  def spec =
    suite("ZConnectionPoolSpec") {
      suite("pool") {
        test("creation") {
          for {
            _ <- ZConnectionPool.h2test.build.use(_ => ZIO.unit)
          } yield assertCompletes
        }
      } +
        suite("sql") {
          test("create table") {
            for {
              _ <- createUsers
            } yield assertCompletes
          } +
            test("insert") {
              for {
                _   <- createUsers
                num <- insertSherlock
              } yield assertTrue(num == 1L)
            } +
            test("select one") {
              for {
                _     <- createUsers *> insertSherlock
                value <- transaction {
                           selectOne {
                             sql"select name, age from users where name = ${sherlockHolmes.name}".as[User]
                           }
                         }
              } yield assertTrue(value == Some(sherlockHolmes))
            } +
            test("select all") {
              for {
                _     <- createUsers *> insertSherlock
                value <- transaction {
                           selectAll {
                             sql"select name, age from users where name = ${sherlockHolmes.name}".as[User]
                           }
                         }
              } yield assertTrue(value == Chunk(sherlockHolmes))
            } +
            test("select stream") {
              for {
                _     <- createUsers *> insertSherlock
                value <- transaction {
                           selectStream {
                             sql"select name, age from users where name = ${sherlockHolmes.name}".as[User]
                           }.runCollect
                         }
              } yield assertTrue(value == Chunk(sherlockHolmes))
            } +
            test("delete") {
              for {
                _   <- createUsers *> insertSherlock
                num <- transaction(delete(sql"delete from users where name = ${sherlockHolmes.name}"))
              } yield assertTrue(num == 1L)
            } +
            test("update") {
              for {
                _   <- createUsers *> insertSherlock
                num <- transaction(update(sql"update users set age = 43 where name = ${sherlockHolmes.name}"))
              } yield assertTrue(num == 1L)
            }
        } +
        suite("decoding") {
          test("schema-derived") {
            for {
              _     <- createUsers *> insertSherlock
              value <- transaction {
                         selectOne {
                           sql"select name, age from users where name = ${sherlockHolmes.name}".as[Person](
                             JdbcDecoder.fromSchema(Person.schema)
                           )
                         }
                       }
            } yield assertTrue(value == Some(Person(sherlockHolmes.name, sherlockHolmes.age)))
          }
        }
    }.provideCustomLayer(ZConnectionPool.h2test.orDie) @@ sequential
}
