package zio.jdbc

import zio.test._

object SqlSpec extends ZIOSpecDefault {
  def spec: ZSpec[Environment with TestEnvironment, Any] =
    suite("SqlSpec") {
      test("constant") {
        assertTrue(sql"""null""".toString() == "Sql(null)")
      } +
        test("1 param") {
          val id = "sholmes"

          assertTrue(
            sql"select name, age from users where id = ${id}".toString ==
              s"Sql(select name, age from users where id = ?, $id)"
          )
        } +
        test("ensure no empty Syntax instances") {
          val age  = 42
          val name = "sholmes"
          assertTrue(sql"select name, age from users where age = $age and name = $name".segments.size == 4)
        }
    }
}
