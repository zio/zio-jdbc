package zio.jdbc

import zio.test._

object SqlSpec extends ZIOSpecDefault {
  def spec: ZSpec[Environment with TestEnvironment, Any] =
    suite("SqlSpec") {
      test("constant") {
        assertTrue(sql"null".toString() == "Sql(null)")
      } +
        test("1 param") {
          val id = "sholmes"

          assertTrue(
            sql"select name, age from users where id = $id".toString ==
              s"Sql(select name, age from users where id = ?, $id)"
          )
        } +
        suite("operators") {
          val id   = "foo"
          val name = "bar"
          test("values") {
            assertTrue(
              sql"insert into users (name, age)".values(("x", 1), ("y", 2)).toString ==
                s"Sql(insert into users (name, age) VALUES (?,?),(?,?), x, 1, y, 2)"
            )
          } +
            suite("or") {
              test("single arg")(
                assertTrue(("a" or "b").toString() == s"Sql(a OR b)")
              ) +
                test("single arg with param") {
                  assertTrue(
                    (sql"id = $id" or sql"name = $name").toString() ==
                      s"Sql(id = ? OR name = ?, $id, $name)"
                  )
                } +
                test("multiple args") {
                  val email = "a@b.c"
                  assertTrue(
                    sql"select name, age from users where name = $name"
                      .or(sql"id = $id", sql"email = $email")
                      .toString ==
                      s"Sql(select name, age from users where name = ? OR id = ? OR email = ?, $name, $id, $email)"
                  )
                }
            } + suite("and") {
              test("single arg")(
                assertTrue(("a" and "b").toString() == s"Sql(a AND b)")
              ) +
                test("single arg with param") {
                  assertTrue(
                    (sql"id = $id" and sql"name = $name").toString() ==
                      s"Sql(id = ? AND name = ?, $id, $name)"
                  )
                } +
                test("multiple args") {
                  val email = "a@b.c"
                  assertTrue(
                    sql"select name, age from users where name = $name"
                      .and(sql"id = $id", sql"email = $email")
                      .toString ==
                      s"Sql(select name, age from users where name = ? AND id = ? AND email = ?, $name, $id, $email)"
                  )
                }
            } +
            test("not") {
              assertTrue(
                sql"select name, age from users where"
                  .not(sql"id = $id")
                  .toString ==
                  s"Sql(select name, age from users where NOT id = ?, $id)"
              )
            } +
            test("where") {
              assertTrue(
                sql"select name, age from users"
                  .where(sql"id = $id")
                  .toString ==
                  s"Sql(select name, age from users WHERE id = ?, $id)"
              )
            } +
            test("in") {
              assertTrue(
                sql"select name, age from users where id"
                  .in(1, 2, 3)
                  .toString ==
                  s"Sql(select name, age from users where id IN (?,?,?), 1, 2, 3)"
              )
            } +
            test("not in") {
              assertTrue(
                sql"select name, age from users where id"
                  .notIn(1, 2, 3)
                  .toString ==
                  s"Sql(select name, age from users where id NOT IN (?,?,?), 1, 2, 3)"
              )
            }
        }
    }
}
