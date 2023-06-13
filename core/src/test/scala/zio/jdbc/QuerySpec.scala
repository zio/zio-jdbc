package zio.jdbc

import zio._
import zio.test._

import scala.util.Random
object QuerySpec extends ZIOSpecDefault {

  final case class User(name: String, age: Int)

  object User {
    implicit val jdbcDecoder: JdbcDecoder[User] =
      JdbcDecoder[(String, Int)]().map[User](t => User(t._1, t._2))

    implicit val jdbcEncoder: JdbcEncoder[User] = (value: User) => {
      val name = value.name
      val age  = value.age
      sql"""${name}""" ++ ", " ++ s"${age}"
    }
  }

  def spec: Spec[TestEnvironment, Any] =
    suite("QuerySpec Unit") {
      def testConnection(failNext: Boolean, elems: Int) =
        new ZConnection(new ZConnection.Restorable(new TestConnection(failNext, elems)))
      val tableName                                     = sql"users"
      val fields                                        = SqlFragment("id, name, age")
      val testSql                                       = sql"select $fields from $tableName"
      val query: Query[User]                            = testSql.query[User]
      test("Query Success ResultSet Automatic Close") {
        val elements = 10
        ZIO.scoped {
          for {
            rsClosedTuple <- ZIO.scoped {
                               for {
                                 rs     <- query.executeQuery(testSql)
                                 closed <- rs.access(_.isClosed())
                                 count   = {
                                   var c = 0
                                   while (rs.next()) c += 1
                                   c
                                 }
                               } yield (rs, closed, count)
                             }
            closed        <- rsClosedTuple._1.access(_.isClosed())
          } yield assertTrue(closed && !rsClosedTuple._2 && rsClosedTuple._3 == elements)
        }.provide(ZLayer.succeed(testConnection(false, elements)))
      } +
        test("Query ResultSet Iteration Fail Automatic Close") {
          val elements = 10
          ZIO.scoped {
            for {
              rsClosedTuple <- ZIO.scoped {
                                 for {
                                   rs     <- query.executeQuery(testSql)
                                   closed <- rs.access(_.isClosed())
                                   failed <- ZIO.attempt {
                                               rs.next()
                                               false
                                             }.orElseSucceed(true)
                                 } yield (rs, closed, failed)
                               }
              closed        <- rsClosedTuple._1.access(_.isClosed())
            } yield assertTrue(closed && !rsClosedTuple._2, rsClosedTuple._3)
          }.provide(ZLayer.succeed(testConnection(failNext = true, elems = elements)))
        }
    } +
      suite("QuerySpec Integration") {

        final case class UserNoId(name: String, age: Int)

        object UserNoId {
          implicit val jdbcDecoder: JdbcDecoder[UserNoId] =
            JdbcDecoder[(String, Int)]().map[UserNoId](t => UserNoId(t._1, t._2))

          implicit val jdbcEncoder: JdbcEncoder[UserNoId] = (value: UserNoId) => {
            val name = value.name
            val age  = value.age
            sql"""${name}""" ++ ", " ++ s"${age}"
          }
        }

        def createTableUsers =
          sql"""
          create table users (
            id identity primary key,
            name varchar not null,
            age int not null
            )""".execute

        def genUser: UserNoId = {
          val name = Random.nextString(8)
          val id   = Random.nextInt(100000)
          UserNoId(name, id)
        }

        def genUsers(size: Int): List[UserNoId] = List.fill(size)(genUser)

        def insertEverything(elems: Int): ZIO[ZConnection, Throwable, Long] = {
          val users           = genUsers(elems)
          val insertStatement = SqlFragment.insertInto("users")("name", "age").values(users)
          for {
            inserted <- insertStatement.insert
          } yield inserted
        }

        def liveConnection =
          for {
            _       <- ZIO.attempt(Class.forName("org.h2.Driver"))
            int     <- zio.Random.nextInt
            acquire <- ZIO.attemptBlocking {
                         java.sql.DriverManager.getConnection(s"jdbc:h2:mem:test_database_$int")
                       }
          } yield (new ZConnection(new ZConnection.Restorable(acquire)))

        test("Query Success ResultSet Automatic Close") {

          ZIO.scoped {

            val tableName          = sql"users"
            val fields             = SqlFragment("id, name, age")
            val testSql            = sql"select $fields from $tableName"
            val query: Query[User] = testSql.query[User]
            val elements           = 3000
            for {
              _             <- createTableUsers
              _             <- insertEverything(elements)
              rsClosedTuple <- ZIO.scoped {
                                 for {
                                   rs     <- query.executeQuery(testSql)
                                   closed <- rs.access(_.isClosed())
                                   count   = {
                                     var c = 0
                                     while (rs.next()) c += 1
                                     c
                                   }
                                 } yield (rs, closed, count)
                               }
              closed        <- rsClosedTuple._1.access(_.isClosed())
            } yield assertTrue(
              closed && !rsClosedTuple._2 && rsClosedTuple._3 == elements
            ) //Assert ResultSet is closed Outside scope but was open inside scope
          }.provide(ZLayer.fromZIO(liveConnection))
        }
      }
}
