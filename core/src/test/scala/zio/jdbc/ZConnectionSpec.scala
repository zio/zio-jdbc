package zio.jdbc

import zio.test._
import zio.{ Random, ZIO }

import java.sql.PreparedStatement

object ZConnectionSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment, Any] =
    suite("ZConnectionSpec TestConnection") {
      def testConnection = new ZConnection(new ZConnection.Restorable(new TestConnection))

      test("PreparedStatement Automatic Close Normal") {
        ZIO.scoped {
          for {
            statementClosedTuple <- testConnection.executeSqlWith(
                                      sql"""
                create table users_no_id (
                name varchar not null,
                age int not null
                )""",
                                      false
                                    )(ps => ZIO.succeed((ps, ps.isClosed())))
          } yield assertTrue(statementClosedTuple._1.isClosed() && !statementClosedTuple._2)
        }
      } +
        test("PreparedStatement Automatic Close Fail") {
          ZIO.scoped {
            for {
              res                  <- testConnection
                                        .executeSqlWith(
                                          sql"""
                create table users_no_id (
                name varchar not null,
                age int not null
                )""",
                                          false
                                        )(ps => ZIO.succeed(new DummyException("Error Ocurred", ps, ps.isClosed())))
              statementClosedTuple <- ZIO.succeed((res.preparedStatement, res.closedInScope))
            } yield assertTrue(statementClosedTuple._1.isClosed() && !statementClosedTuple._2)
          } //A bit of a hack, DummyException receives the prepared Statement so that its closed State can be checked outside ZConnection's Scope
        }
    } +
      suite("ZConnectionSpec LiveConnection") {

        def liveConnection = for {
          _       <- ZIO.attempt(Class.forName("org.h2.Driver"))
          int     <- Random.nextInt
          acquire <- ZIO.attemptBlocking {
                       java.sql.DriverManager.getConnection(s"jdbc:h2:mem:test_database_$int")
                     }
        } yield (new ZConnection(new ZConnection.Restorable(acquire)))

        test("PreparedStatement Automatic Close Normal") {
          ZIO.scoped {
            for {
              conn                 <- liveConnection
              statementClosedTuple <- conn.executeSqlWith(
                                        sql"""
                create table users_no_id (
                name varchar not null,
                age int not null
                )""",
                                        false
                                      )(ps => ZIO.succeed((ps.executeUpdate(), ps, ps.isClosed())))
            } yield assertTrue(
              statementClosedTuple._1 == 0 &&
                statementClosedTuple._2.isClosed() && !statementClosedTuple._3
            )
          }
        }
      }

  case class DummyException(msg: String, val preparedStatement: PreparedStatement, val closedInScope: Boolean)
      extends Exception(msg)

}
