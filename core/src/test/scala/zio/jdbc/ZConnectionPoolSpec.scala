package zio.jdbc

import zio._
import zio.jdbc.SqlFragment.Setter
import zio.schema._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import java.sql.Connection
import scala.util.Random

object ZConnectionPoolSpec extends ZIOSpecDefault {
  final case class Person(name: String, age: Int)

  object Person {

    import Schema.Field

    implicit val schema: Schema[Person] =
      Schema.CaseClass2[String, Int, Person](
        TypeId.parse(classOf[Person].getName),
        Field("name", Schema[String], get0 = _.name, set0 = (x, v) => x.copy(name = v)),
        Field("age", Schema[Int], get0 = _.age, set0 = (x, v) => x.copy(age = v)),
        Person.apply
      )
  }

  val sherlockHolmes: User = User("Sherlock Holmes", 42)
  val johnWatson: User     = User("John Watson", 40)
  val johnDoe: User        = User("John Doe", 18)

  val user1: UserNoId = UserNoId("User 1", 3)
  val user2: UserNoId = UserNoId("User 2", 4)
  val user3: UserNoId = UserNoId("John Watson II", 32)
  val user4: UserNoId = UserNoId("John Watson III", 98)
  val user5: UserNoId = UserNoId("Sherlock Holmes II", 2)

  def genUser: UserNoId = {
    val name = Random.nextString(8)
    val id   = Random.nextInt(100000)
    UserNoId(name, id)
  }

  def genUsers(size: Int): List[UserNoId] =
    List.fill(size)(genUser)

  val createUsers: ZIO[ZConnectionPool with Any, Throwable, Unit] =
    transaction {
      sql"""
      create table users (
        id identity primary key,
        name varchar not null,
        age int not null
      )
      """.execute
    }

  val createUsersNoId: ZIO[ZConnectionPool with Any, Throwable, Unit] = transaction {
    sql"""
    create table users_no_id (
        name varchar not null,
        age int not null
    )
     """.execute
  }

  val insertSherlock: ZIO[ZConnectionPool with Any, Throwable, UpdateResult[Long]] =
    transaction {
      sql"insert into users values (default, ${sherlockHolmes.name}, ${sherlockHolmes.age})".insertWithKeys
    }

  val insertWatson: ZIO[ZConnectionPool with Any, Throwable, UpdateResult[Long]] =
    transaction {
      sql"insert into users values (default, ${johnWatson.name}, ${johnWatson.age})".insertWithKeys
    }

  val insertJohn: ZIO[ZConnectionPool with Any, Throwable, UpdateResult[Long]] =
    transaction {
      sql"insert into users values (default, ${johnDoe.name}, ${johnDoe.age})".insertWithKeys
    }

  val insertBatches: ZIO[ZConnectionPool, Throwable, Long] = transaction {
    val users  = genUsers(10000).toSeq
    val mapped = users.map(SqlFragment.insertInto("users_no_id")("name", "age").values(_))
    for {
      inserted <- ZIO.foreach(mapped)(_.insertWithKeys)
    } yield inserted.map(_.rowsUpdated).sum
  }

  val insertFive: ZIO[ZConnectionPool, Throwable, Long] = transaction {
    val users           = Seq(user1, user2, user3, user4, user5)
    val insertStatement = SqlFragment.insertInto("users_no_id")("name", "age").values(users)
    for {
      inserted <- insertStatement.insertWithKeys
    } yield inserted.rowsUpdated
  }

  val insertEverything: ZIO[ZConnectionPool, Throwable, Long] = transaction {
    val users           = genUsers(3000)
    val insertStatement = SqlFragment.insertInto("users_no_id")("name", "age").values(users)
    for {
      inserted <- insertStatement.insertWithKeys
    } yield inserted.rowsUpdated
  }

  final case class User(name: String, age: Int)

  object User {
    implicit val jdbcDecoder: JdbcDecoder[User] =
      JdbcDecoder[(String, Int)].map[User](t => User(t._1, t._2))

    implicit val jdbcEncoder: JdbcEncoder[User] = (value: User) => {
      val name = value.name
      val age  = value.age
      sql"""${name}""" ++ ", " ++ s"${age}"
    }
  }

  final case class UserNoId(name: String, age: Int)

  object UserNoId {
    implicit val jdbcDecoder: JdbcDecoder[UserNoId] =
      JdbcDecoder[(String, Int)].map[UserNoId](t => UserNoId(t._1, t._2))

    implicit val jdbcEncoder: JdbcEncoder[UserNoId] = (value: UserNoId) => {
      val name = value.name
      val age  = value.age
      sql"""${name}""" ++ ", " ++ s"${age}"
    }
  }

  def spec: Spec[TestEnvironment, Any] =
    suite("ZConnectionPoolSpec") {
      def testPool(config: ZConnectionPoolConfig = ZConnectionPoolConfig.default) =
        for {
          conns  <- Queue.unbounded[TestConnection]
          getConn = ZIO.succeed(new TestConnection).tap(conns.offer(_))
          pool   <- ZLayer.succeed(config).to(ZConnectionPool.make(getConn)).build.map(_.get)
        } yield conns -> pool

      test("make") {
        ZIO.scoped {
          for {
            _ <- testPool()
          } yield assertCompletes
        }
      } +
        test("transaction") {
          ZIO.scoped {
            for {
              pool <- testPool().map(_._2)
              conn <- pool.transaction.build.map(_.get)
              connClosed = conn.underlying.isClosed // temp workaround for assertTrue eval out of scope
            } yield assertTrue(!connClosed)
          }
        } +
        test("invalidate close connection") {
          ZIO.scoped {
            for {
              pool <- testPool().map(_._2)
              conn <- ZIO.scoped(for {
                        conn <- pool.transaction.build.map(_.get)
                        _    <- pool.invalidate(conn)
                      } yield conn.underlying)
              invalidatedClosed = conn.isClosed // temp workaround for assertTrue eval out of scope
            } yield assertTrue(invalidatedClosed)
          }
        } +
        test("auto invalidate dead connections") {
          def testPoolSingle = testPool(
            ZConnectionPoolConfig(1, 1, ZConnectionPoolConfig.defaultRetryPolicy, 300.seconds)
          ).map(_._2) //Pool with only one connection

          ZIO.scoped {
            for {
              pool       <- testPoolSingle
              conn       <- ZIO.scoped(for {
                              conn <- pool.transaction.build.map(_.get)
                              _    <- conn.close
                            } yield conn.underlying)
              invalidatedClosed = conn.isClosed // temp workaround for assertTrue eval out of scope
              conn2      <- pool.transaction.build.map(_.get)
              conn2Closed = conn2.underlying.isClosed
            } yield assertTrue(
              invalidatedClosed && !conn2Closed
            )
          }
        } +
        test("shutdown closes all conns") {
          ZIO.scoped {
            for {
              t1                          <- testPool()
              (conns, pool)                = t1
              conn                        <- pool.transaction.build.map(_.get)
              isClosedBeforePoolShutdown   = conn.underlying.isClosed
              isClosedAfterPoolShutdownZIO = ZIO.succeed(conn.underlying.isClosed)
            } yield (isClosedBeforePoolShutdown, isClosedAfterPoolShutdownZIO, conns.takeAll)
          }.flatMap { case (isClosedBeforePoolShutdown, isClosedAfterPoolShutdownZIO, allConnsZIO) =>
            for {
              allConns                  <- allConnsZIO
              isClosedAfterPoolShutdown <- isClosedAfterPoolShutdownZIO
            } yield assertTrue(!isClosedBeforePoolShutdown && isClosedAfterPoolShutdown && allConns.forall(_.isClosed))
          }
        } @@ nonFlaky +
        test("restore") {
          ZIO.scoped {
            for {
              tuple        <- testPool(ZConnectionPoolConfig.default.copy(minConnections = 1, maxConnections = 1))
              (conns, pool) = tuple
              _            <- ZIO.scoped {
                                pool.transaction.build.map(_.get).tap { connection =>
                                  ZIO.succeed {
                                    connection.underlying.setAutoCommit(false)
                                    connection.underlying.setCatalog("catalog")
                                    connection.underlying.setClientInfo("clientInfo", "clientInfoValue")
                                    connection.underlying.setReadOnly(true)
                                    connection.underlying.setSchema("schema")
                                    connection.underlying.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE)
                                  }
                                }
                              }
              conn         <- conns.take
              autoCommit   <- ZIO.succeed(conn.getAutoCommit)
              catalog      <- ZIO.succeed(conn.getCatalog)
              clientInfo   <- ZIO.succeed(conn.getClientInfo("clientInfo"))
              readOnly     <- ZIO.succeed(conn.isReadOnly)
              schema       <- ZIO.succeed(conn.getSchema)
              isolation    <- ZIO.succeed(conn.getTransactionIsolation)
            } yield assertTrue(autoCommit) &&
              assertTrue(catalog == "") &&
              assertTrue(clientInfo == null) &&
              assertTrue(readOnly == false) &&
              assertTrue(schema == "") &&
              assertTrue(isolation == Connection.TRANSACTION_NONE)
          }
        }
    } +
      suite("ZConnectionPoolSpec integration tests") {
        suite("pool") {
          test("creation") {
            for {
              _ <- ZIO.scoped(ZConnectionPool.h2test.build)
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
                  _      <- createUsers
                  result <- insertSherlock
                } yield assertTrue(result.rowsUpdated == 1L) && assertTrue(result.updatedKeys.nonEmpty)
              } +
              test("insertBatch of 10000") {
                for {
                  _      <- createUsersNoId
                  result <- insertBatches
                } yield assertTrue(result == 10000)
              } +
              test("select one") {
                for {
                  _     <- createUsers *> insertSherlock
                  value <- transaction {
                             sql"select name, age from users where name = ${sherlockHolmes.name}".query[User].selectOne
                           }
                } yield assertTrue(value.contains(sherlockHolmes))
              } +
              test("select all") {
                for {
                  _     <- createUsers *> insertSherlock *> insertWatson
                  value <- transaction {
                             sql"select name, age from users".query[User].selectAll
                           }
                } yield assertTrue(value == Chunk(sherlockHolmes, johnWatson))
              } +
              test("select all in") {
                val namesToSearch = Chunk(sherlockHolmes.name, johnDoe.name)

                def assertUsersFound[A: Setter](collection: A) =
                  for {
                    users <- transaction {
                               sql"select name, age from users where name IN ($collection)".query[User].selectAll
                             }
                  } yield assertTrue(
                    users.map(_.name) == namesToSearch
                  )

                def asserttions =
                  assertUsersFound(namesToSearch) &&
                    assertUsersFound(namesToSearch.toList) &&
                    assertUsersFound(namesToSearch.toVector) &&
                    assertUsersFound(namesToSearch.toSet)

                for {
                  _          <- createUsers *> insertSherlock *> insertWatson *> insertJohn
                  testResult <- asserttions
                } yield testResult
              } +
              test("select all multiple in") {
                val names1        = Vector(sherlockHolmes.name, johnWatson.name)
                val names2        = Chunk(johnDoe.name)
                val namesToSearch = Chunk.fromIterable(names1) ++ names2

                for {
                  _     <- createUsers *> insertSherlock *> insertWatson *> insertJohn
                  users <- transaction {
                             sql"select name, age from users where name IN ($names1) OR name in ($names2)"
                               .query[User]
                               .selectAll
                           }
                } yield assertTrue(users.map(_.name) == namesToSearch)
              } +
              test("select all in empty") {
                val empty = Chunk.empty[String]

                for {
                  _     <- createUsers *> insertSherlock
                  users <- transaction {
                             sql"select name, age from users where name IN ($empty)"
                               .query[User]
                               .selectAll
                           }
                } yield assertTrue(users.isEmpty)
              } +
              test("select stream") {
                for {
                  _     <- createUsersNoId *> insertFive
                  value <- transaction {
                             sql"select name, age from users_no_id".query[UserNoId].selectStream(2).chunks.runCollect
                           }
                } yield assertTrue(value == Chunk(Chunk(user1, user2), Chunk(user3, user4), Chunk(user5)))
              } +
              test("delete") {
                for {
                  _   <- createUsers *> insertSherlock
                  num <- transaction(sql"delete from users where name = ${sherlockHolmes.name}".delete)
                } yield assertTrue(num == 1L)
              } +
              test("update") {
                for {
                  _   <- createUsers *> insertSherlock
                  num <- transaction(sql"update users set age = 43 where name = ${sherlockHolmes.name}".update)
                } yield assertTrue(num == 1L)
              }
          } +
          suite("decoding") {
            test("schema-derived") {
              for {
                _     <- createUsers *> insertSherlock
                value <- transaction {
                           sql"select name, age from users where name = ${sherlockHolmes.name}"
                             .query[Person](
                               JdbcDecoder.fromSchema(Person.schema)
                             )
                             .selectOne
                         }
              } yield assertTrue(value.contains(Person(sherlockHolmes.name, sherlockHolmes.age)))
            }
          } +
          suite("transaction layer finalizer") {
            test("do not rollback when autoCommit = true") {
              // `ZIO.addFinalizerExit` doesn't allow throwing exceptions, so the only way to check
              // if rollback failed is to check if there is a log added by `ignoreLogged`.
              for {
                // H2 won't throw an exception on rollback without this property being set to true.
                _          <- ZIO.attempt(java.lang.System.setProperty("h2.forceAutoCommitOffOnCommit", "true"))
                result     <- transaction {
                                for {
                                  conn   <- ZIO.service[ZConnection]
                                  _      <- conn.access(_.setAutoCommit(true))
                                  result <- sql"select * from non_existent_table".execute
                                } yield result
                              }.either
                logEntries <- ZTestLogger.logOutput
                logMessages = logEntries.map(_.message())
              } yield assertTrue(
                !logMessages.contains("An error was silently ignored because it is not anticipated to be useful")
              ) &&
                assert(logMessages.size)(equalTo(1)) &&
                assert(result)(isLeft)
            }
          }
      }.provide(ZConnectionPool.h2test.orDie) @@ sequential

}
