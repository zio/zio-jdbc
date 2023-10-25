package zio.jdbc

import zio.test._

object ReturningSpec extends PgSpec {
  case class User(name: String, age: Int)

  object User {
    implicit val jdbcDecoder: JdbcDecoder[User] =
      JdbcDecoder[(String, Int)].map((User.apply _).tupled)
    implicit val jdbcEncoder: JdbcEncoder[User] =
      JdbcEncoder[(String, Int)].contramap(User.unapply(_).get)
  }

  val spec =
    suite("Returning")(
      test("Inserts returning rows") {
        check(Gen.chunkOf1(Gen.alphaNumericString.map(_.take(40)).zip(Gen.int(10, 100)))) { tuples =>
          val users = tuples.map((User.apply _).tupled).toChunk

          for {
            result <- transaction {
                        (sql"""INSERT INTO users(name,age)""".values(users) ++ " RETURNING id, name, age")
                          .insertReturning[(Int, String, Int)]
                      }
            _      <- transaction(sql"DELETE FROM users".delete)
          } yield assert(result.rowsUpdated)(Assertion.equalTo(users.size.toLong)) &&
            assert(result.updatedKeys.map(_._1).toSet)(Assertion.hasSameElements(result.updatedKeys.map(_._1)))
        }
      },
      test("Updates returning rows") {
        check(Gen.chunkOf1(Gen.alphaNumericString.map(_.take(40)).zip(Gen.int(10, 100)))) { tuples =>
          val users = tuples.map((User.apply _).tupled).toChunk

          for {
            _      <- transaction {
                        sql"""INSERT INTO users(name,age)""".values(users).execute
                      }
            result <- transaction {
                        sql"""UPDATE users SET age = age * 2 RETURNING name, age""".updateReturning[User]
                      }
            _      <- transaction(sql"DELETE FROM users".delete)
          } yield assert(result.rowsUpdated)(Assertion.equalTo(users.size.toLong)) &&
            assert(result.updatedKeys)(Assertion.hasSameElements(users.map(u => u.copy(age = u.age * 2))))
        }
      } @@ TestAspect.samples(2),
      test("Deletes returning rows") {
        check(Gen.chunkOf1(Gen.alphaNumericString.map(_.take(40)).zip(Gen.int(10, 100)))) { tuples =>
          val users = tuples.map((User.apply _).tupled).toChunk

          for {
            _      <- transaction {
                        sql"""INSERT INTO users(name,age)""".values(users).execute
                      }
            result <- transaction {
                        sql"""DELETE FROM users RETURNING name, age""".deleteReturning[User]
                      }
          } yield assert(result.rowsUpdated)(Assertion.equalTo(users.size.toLong)) &&
            assert(result.updatedKeys)(Assertion.hasSameElements(users))
        }
      }
    ) @@ TestAspect.sequential @@ TestAspect.around(
      transaction(sql"CREATE TABLE users (id SERIAL, name VARCHAR(40), age INTEGER)".execute),
      transaction(sql"DROP TABLE users".execute).orDie
    )
}
