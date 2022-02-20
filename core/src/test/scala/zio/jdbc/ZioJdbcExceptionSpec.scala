package zio.jdbc

import zio.test.Assertion._
import zio.test.TestAspect.sequential
import zio.test._

object ZioJdbcExceptionSpec extends ZIOSpecDefault {
  val defectiveSql: SqlFragment =
    sql"""
      create table users (
        id identity primary key,
        name varchar not null,
        age int not null
      """ // missing closing parenthesis

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("ZioJdbcExceptionSpec")(
      test("transaction fails with ZioJdbcException and contains original sql query") {
        for {
          res <- transaction(execute(defectiveSql)).exit
        } yield assert(res)(
          fails(isSubtype[ZioJdbcException](anything) && hasMessage(containsString(defectiveSql.toString())))
        )
      }
    ).provideCustomLayer(ZConnectionPool.h2test.orDie) @@ sequential

}
