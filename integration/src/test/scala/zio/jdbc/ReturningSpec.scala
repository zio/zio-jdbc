package zio.jdbc

import zio.Scope
import zio.test._

import java.util.UUID

object ReturningSpec extends PgSpec {
  final case class User(internalId: UUID, name: String, age: Int)

  object User {
    implicit val jdbcDecoder: JdbcDecoder[User] =
      JdbcDecoder[(UUID, String, Int)].map((User.apply _).tupled)
    implicit val jdbcEncoder: JdbcEncoder[User] =
      JdbcEncoder[(UUID, String, Int)].contramap(User.unapply(_).get)
  }

  val genUser: Gen[Any, User] =
    for {
      uuid <- Gen.uuid
      name <- Gen.alphaNumericString.map(_.take(40))
      age  <- Gen.int(10, 100)
    } yield User(internalId = uuid, name = name, age = age)

  val spec: Spec[ZConnectionPool with TestEnvironment with Scope, Any] =
    suite("Returning")(
      test("Inserts returning rows") {
        check(Gen.chunkOf1(genUser)) { users =>
          for {
            result <- transaction {
                        (sql"""INSERT INTO users(internalId, name, age)""".values(
                          users.toChunk
                        ) ++ " RETURNING id, internalId, name, age")
                          .insertReturning[(Int, UUID, String, Int)]
                      }
            _      <- transaction(sql"DELETE FROM users".delete)
          } yield assert(result.rowsUpdated)(Assertion.equalTo(users.size.toLong)) &&
            assert(result.updatedKeys.map(_._1).toSet)(Assertion.hasSameElements(result.updatedKeys.map(_._1)))
        }
      },
      test("Updates returning rows") {
        check(Gen.chunkOf1(genUser)) { users =>
          for {
            _      <- transaction {
                        sql"""INSERT INTO users(internalId, name, age)""".values(users.toChunk).execute
                      }
            result <- transaction {
                        sql"""UPDATE users SET age = age * 2 RETURNING internalId, name, age""".updateReturning[User]
                      }
            _      <- transaction(sql"DELETE FROM users".delete)
          } yield assert(result.rowsUpdated)(Assertion.equalTo(users.size.toLong)) &&
            assert(result.updatedKeys)(Assertion.hasSameElements(users.map(u => u.copy(age = u.age * 2))))
        }
      } @@ TestAspect.samples(2),
      test("Deletes returning rows") {
        check(Gen.chunkOf1(genUser)) { users =>
          for {
            _      <- transaction {
                        sql"""INSERT INTO users(internalId, name, age)""".values(users.toChunk).execute
                      }
            result <- transaction {
                        sql"""DELETE FROM users RETURNING internalId, name, age""".deleteReturning[User]
                      }
          } yield assert(result.rowsUpdated)(Assertion.equalTo(users.size.toLong)) &&
            assert(result.updatedKeys)(Assertion.hasSameElements(users))
        }
      }
    ) @@ TestAspect.sequential @@ TestAspect.around(
      transaction(sql"CREATE TABLE users (id SERIAL, internalId UUID, name VARCHAR(40), age INTEGER)".execute),
      transaction(sql"DROP TABLE users".execute).orDie
    )
}
