package zio.jdbc

import zio._
import zio.schema._
import zio.test.TestAspect._
import zio.test._

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
  val sherlockHolmes: User = User("Sherlock Holmes", 42)

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

  val insertSherlock: ZIO[ZConnectionPool with Any, Throwable, Long] =
    transaction {
      insert {
        sql"insert into users values (default, ${sherlockHolmes.name}, ${sherlockHolmes.age})"
      }
    }

  final case class User(name: String, age: Int)
  object User {
    implicit val jdbcDecoder: JdbcDecoder[User] =
      JdbcDecoder[(String, Int)]().map[User](t => User(t._1, t._2))
  }

  def spec: ZSpec[TestEnvironment, Any] =
    suite("ZConnectionPoolSpec") {
      suite("pool") {
        test("creation") {
          for {
            _ <- ZIO.scoped(ZConnectionPool.h2test.build)
          } yield assertCompletes
        }
        // TODO can we check that the connection is invalidated within underlying ZPool?
        test("invalidate a connection") {
          for {
            zcp     <- ZIO.service[ZConnectionPool]
            env     <- ZIO.scoped(zcp.transaction.build)
            con      = env.get[ZConnection]
            _       <- zcp.invalidate(con)
            isValid <- con.isValid()
          } yield assertTrue(!isValid)
        }
      }.provideCustomLayer(ZConnectionPool.h2test.orDie) +
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
              } yield assertTrue(value.contains(sherlockHolmes))
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
            } yield assertTrue(value.contains(Person(sherlockHolmes.name, sherlockHolmes.age)))
          }
        }
    }.provideCustomLayer(ZConnectionPool.h2test.orDie) @@ sequential
}
