package zio.jdbc

import zio._
import zio.schema._
import zio.test.TestAspect._
import zio.test._

import java.sql.{
  Blob,
  CallableStatement,
  Clob,
  Connection,
  DatabaseMetaData,
  NClob,
  PreparedStatement,
  SQLWarning,
  SQLXML,
  Savepoint,
  Statement,
  Struct
}
import java.util.{ Properties, concurrent }
import java.{ sql, util }

object ZConnectionPoolSpec extends ZIOSpecDefault {
  final case class Person(name: String, age: Int)

  object Person {
    import Schema.Field

    implicit val schema: Schema[Person] =
      Schema.CaseClass2[String, Int, Person](
        TypeId.parse(classOf[Person].getName),
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

  def spec: Spec[TestEnvironment, Any] =
    suite("ZConnectionPoolSpec") {
      def testPool(config: ZConnectionPoolConfig = ZConnectionPoolConfig.default) = for {
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
              connClosed = conn.connection.isClosed // temp workaround for assertTrue eval out of scope
            } yield assertTrue(!connClosed)
          }
        } +
        test("invalidate close connection") {
          val poolConfig = ZConnectionPoolConfig.default
          ZIO.scoped {
            for {
              pool             <- testPool().map(_._2)
              conn             <- pool.transaction.build.map(_.get)
              _                <- pool.invalidate(conn)
              invalidatedClosed = conn.connection.isClosed
              _                <- ZIO.scoped(pool.transaction.build).repeatN(poolConfig.maxConnections)
              expectClosed = conn.connection.isClosed // temp workaround for assertTrue eval out of scope
            } yield assertTrue(!invalidatedClosed && expectClosed)
          }
        } +
        test("shutdown closes all conns") {
          ZIO.scoped {
            for {
              t1                          <- testPool()
              (conns, pool)                = t1
              conn                        <- pool.transaction.build.map(_.get)
              isClosedBeforePoolShutdown   = conn.connection.isClosed
              isClosedAfterPoolShutdownZIO = ZIO.succeed(conn.connection.isClosed)
            } yield (isClosedBeforePoolShutdown, isClosedAfterPoolShutdownZIO, conns.takeAll)
          }.flatMap { case (isClosedBeforePoolShutdown, isClosedAfterPoolShutdownZIO, allConnsZIO) =>
            for {
              allConns                  <- allConnsZIO
              isClosedAfterPoolShutdown <- isClosedAfterPoolShutdownZIO
            } yield assertTrue(!isClosedBeforePoolShutdown && isClosedAfterPoolShutdown && allConns.forall(_.isClosed))
          }
        } @@ nonFlaky
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
      }.provide(ZConnectionPool.h2test.orDie) @@ sequential

  class TestConnection extends Connection {
    private var closed                                                                                               = false
    def close(): Unit                                                                                                = closed = true
    def isClosed: Boolean                                                                                            = closed
    def createStatement(): Statement                                                                                 = ???
    def prepareStatement(sql: String): PreparedStatement                                                             = ???
    def prepareCall(sql: String): CallableStatement                                                                  = ???
    def nativeSQL(sql: String): String                                                                               = ???
    def setAutoCommit(autoCommit: Boolean): Unit                                                                     = ???
    def getAutoCommit: Boolean                                                                                       = ???
    def commit(): Unit                                                                                               = ???
    def rollback(): Unit                                                                                             = ???
    def getMetaData: DatabaseMetaData                                                                                = ???
    def setReadOnly(readOnly: Boolean): Unit                                                                         = ???
    def isReadOnly: Boolean                                                                                          = ???
    def setCatalog(catalog: String): Unit                                                                            = ???
    def getCatalog: String                                                                                           = ???
    def setTransactionIsolation(level: RuntimeFlags): Unit                                                           = ???
    def getTransactionIsolation: RuntimeFlags                                                                        = ???
    def getWarnings: SQLWarning                                                                                      = ???
    def clearWarnings(): Unit                                                                                        = ???
    def createStatement(resultSetType: RuntimeFlags, resultSetConcurrency: RuntimeFlags): Statement                  = ???
    def prepareStatement(
      sql: String,
      resultSetType: RuntimeFlags,
      resultSetConcurrency: RuntimeFlags
    ): PreparedStatement = ???
    def prepareCall(sql: String, resultSetType: RuntimeFlags, resultSetConcurrency: RuntimeFlags): CallableStatement =
      ???
    def getTypeMap: util.Map[String, Class[_]]                                                                       = ???
    def setTypeMap(map: util.Map[String, Class[_]]): Unit                                                            = ???
    def setHoldability(holdability: RuntimeFlags): Unit                                                              = ???
    def getHoldability: RuntimeFlags                                                                                 = ???
    def setSavepoint(): Savepoint                                                                                    = ???
    def setSavepoint(name: String): Savepoint                                                                        = ???
    def rollback(savepoint: Savepoint): Unit                                                                         = ???
    def releaseSavepoint(savepoint: Savepoint): Unit                                                                 = ???
    def createStatement(
      resultSetType: RuntimeFlags,
      resultSetConcurrency: RuntimeFlags,
      resultSetHoldability: RuntimeFlags
    ): Statement = ???
    def prepareStatement(
      sql: String,
      resultSetType: RuntimeFlags,
      resultSetConcurrency: RuntimeFlags,
      resultSetHoldability: RuntimeFlags
    ): PreparedStatement = ???
    def prepareCall(
      sql: String,
      resultSetType: RuntimeFlags,
      resultSetConcurrency: RuntimeFlags,
      resultSetHoldability: RuntimeFlags
    ): CallableStatement = ???
    def prepareStatement(sql: String, autoGeneratedKeys: RuntimeFlags): PreparedStatement                            = ???
    def prepareStatement(sql: String, columnIndexes: Array[RuntimeFlags]): PreparedStatement                         = ???
    def prepareStatement(sql: String, columnNames: Array[String]): PreparedStatement                                 = ???
    def createClob(): Clob                                                                                           = ???
    def createBlob(): Blob                                                                                           = ???
    def createNClob(): NClob                                                                                         = ???
    def createSQLXML(): SQLXML                                                                                       = ???
    def isValid(timeout: RuntimeFlags): Boolean                                                                      = ???
    def setClientInfo(name: String, value: String): Unit                                                             = ???
    def setClientInfo(properties: Properties): Unit                                                                  = ???
    def getClientInfo(name: String): String                                                                          = ???
    def getClientInfo: Properties                                                                                    = ???
    def createArrayOf(typeName: String, elements: Array[AnyRef]): sql.Array                                          = ???
    def createStruct(typeName: String, attributes: Array[AnyRef]): Struct                                            = ???
    def setSchema(schema: String): Unit                                                                              = ???
    def getSchema: String                                                                                            = ???
    def abort(executor: concurrent.Executor): Unit                                                                   = ???
    def setNetworkTimeout(executor: concurrent.Executor, milliseconds: RuntimeFlags): Unit                           = ???
    def getNetworkTimeout: RuntimeFlags                                                                              = ???
    def unwrap[T](iface: Class[T]): T                                                                                = ???
    def isWrapperFor(iface: Class[_]): Boolean                                                                       = ???
  }
}
