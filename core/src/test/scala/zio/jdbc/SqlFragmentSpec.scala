package zio.jdbc

import zio.Chunk
import zio.jdbc.Sql.Setter
import zio.jdbc.{ transaction => transact }
import zio.schema.{ Schema, TypeId }
import zio.test.Assertion._
import zio.test._

import java.sql.SQLException

final case class Person(name: String, age: Int)
final case class UserLogin(username: String, password: String)
final case class ActiveUser(person: Person, login: UserLogin, isActive: Boolean = true)
final case class Transfer(id: Long, amount: Double, location: Option[String])

object SqlFragmentSpec extends ZIOSpecDefault {
  import Models._

  def spec: Spec[Environment with TestEnvironment, Any] =
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
        test("ensure no empty Syntax instances") {
          val age  = 42
          val name = "sholmes"
          assertTrue(sql"select name, age from users where age = $age and name = $name".segments.size == 4)
        } +
        test("interpolate Sql values") {
          val tableName    = sql"table1"
          val (a, b, c, d) = (1, 3, "foo", "bar")
          val filter       = sql"a between $a and $b"
          val fields       = SqlFragment("a, b")
          val testSql      = sql"select $fields from $tableName where c = $c and $filter and d = $d"
          assertTrue(
            testSql.toString == "Sql(select a, b from table1 where c = ? and a between ? and ? and d = ?, foo, 1, 3, bar)"
          )
        } +
        test("type safe interpolation") {
          final case class Foo(value: String)
          implicit val fooParamSetter: SqlFragment.Setter[Foo] = SqlFragment.Setter[String]().contramap(_.toString)

          val testSql = sql"${Foo("test")}"

          assertTrue(testSql.segments.collect { case SqlFragment.Segment.Param(_, setter) =>
            setter
          }.head eq fooParamSetter) &&
          assertTrue(testSql.toString == "Sql(?, Foo(test))")
        } +
        suite(" SqlFragment.ParamSetter instances") { // TODO figure out how to test at PrepareStatement level
          test("Option") {
            val none: Option[Int] = None
            assertTrue(sql"${none}".toString == "Sql(?, None)" && sql"${Option(123)}".toString == "Sql(?, Some(123))")
          }
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
            suite("in") {
              test("fragment method") {
                assertTrue(
                  sql"select name, age from users where id"
                    .in(1, 2, 3)
                    .toString ==
                    "Sql(select name, age from users where id IN (?,?,?), 1, 2, 3)"
                )
              } + test("fragment method where param is iterable") {
                val seq = Seq(1, 2, 3)
                assertTrue(
                  sql"select name, age from users where id"
                    .in(seq)
                    .toString ==
                    "Sql(select name, age from users where id IN (?,?,?), 1, 2, 3)"
                )
              } + test("interpolation param is supported collection") {
                def assertIn[A: Setter](collection: A) = {
                  println(sql"select name, age from users where id in ($collection)".toString)
                  assertTrue(
                    sql"select name, age from users where id in ($collection)".toString ==
                      "Sql(select name, age from users where id in (?,?,?), 1, 2, 3)"
                  )
                }

                assertIn(Chunk(1, 2, 3)) &&
                assertIn(List(1, 2, 3)) &&
                assertIn(Vector(1, 2, 3)) &&
                assertIn(Set(1, 2, 3)) &&
                assertIn(Array(1, 2, 3))
              }
            } +
            test("not in") {
              assertTrue(
                sql"select name, age from users where id"
                  .notIn(1, 2, 3)
                  .toString ==
                  s"Sql(select name, age from users where id NOT IN (?,?,?), 1, 2, 3)"
              )
            } +
            test("tuple values") {
              val person = ("sholmes", 42)
              assertTrue(
                sql"insert into persons (name, age)".values(person).toString ==
                  s"Sql(insert into persons (name, age) VALUES (?,?), ${person._1}, ${person._2})"
              )
            } +
            test("case class values") {

              val person = Person("sholmes", 42)
              assertTrue(
                sql"insert into persons (name, age)".values(person).toString ==
                  s"Sql(insert into persons (name, age) VALUES (?,?), ${person.name}, ${person.age})"
              )
            } +
            test("nested case class values") {
              val person     = Person("Sherlock Holmes", 42)
              val login      = UserLogin("sholmes", "221BakerSt")
              val activeUser = ActiveUser(person, login)
              assertTrue(
                sql"insert into active_users (name, age, username, password, isActive)".values(activeUser).toString ==
                  s"Sql(insert into active_users (name, age, username, password, isActive)" +
                  s" VALUES (?,?,?,?,?), ${activeUser.person.name}, ${activeUser.person.age}, " +
                  s"${login.username}, ${login.password}, ${activeUser.isActive})"
              )
            } +
            test("optional value in case class") {
              val transfer  = Transfer(1, 10.0, None)
              val transfer2 = Transfer(2, 20.0, Some("London"))
              assertTrue(
                sql"insert into transfer (id, amount, location)".values(transfer).toString ==
                  s"Sql(insert into transfer (id, amount, location) VALUES (?,?,NULL), ${transfer.id}, ${transfer.amount})"
              ) &&
              assertTrue(
                sql"insert into transfer (id, amount, location)".values(transfer2).toString ==
                  s"Sql(insert into transfer (id, amount, location) VALUES (?,?,?), ${transfer2.id}, ${transfer2.amount}, ${transfer2.location.get})"
              )
            }
        } +
        test("log SQL errors") {
          val sqlString                 =
            """
              create table users (
                id identity primary key,
                name varchar not null,
                age int not null
              """ // missing closing parenthesis
          val defectiveSql: SqlFragment = stringToSql(sqlString)

          (for {
            res   <- transact(defectiveSql.execute).exit
            error <- ZTestLogger.logOutput.map(_.filter(log => log.logLevel == zio.LogLevel.Debug))
          } yield assert(res)(
            fails(isSubtype[SQLException](anything))
          ) && assert(error.head.annotations.keys)(contains("SQL"))
            && assert(error.head.message())(containsString(sqlString)))
            .provideLayer(ZConnectionPool.h2test.orDie)
        } +
        test(" SqlFragment.select") {
          val result   = SqlFragment.select("name", "age").from("persons")
          val expected = sql"SELECT name, age FROM persons"
          assertTrue(result.toString == expected.toString)
        } +
        test(" SqlFragment.insertInto") {
          val person = ("sholmes", 42)
          val result = SqlFragment.insertInto("persons")("name", "age").values(person)
          assertTrue(
            result.toString ==
              s"Sql(INSERT INTO persons (name, age) VALUES (?,?), ${person._1}, ${person._2})"
          )
        } +
        test(" SqlFragment.deleteFrom") {
          val result = SqlFragment.deleteFrom("persons").where(sql"age < ${21}")
          assertTrue(
            result.toString == s"Sql(DELETE FROM persons WHERE age < ?, 21)"
          )
        } +
        test(" SqlFragment.update") {
          val result = SqlFragment.update("persons")
          assertTrue(
            result.toString == "Sql(UPDATE persons)"
          )
        }

    }
}

object Models {
  import Schema.Field

  implicit val personSchema: Schema[Person] =
    Schema.CaseClass2[String, Int, Person](
      TypeId.parse(classOf[Person].getName),
      Field("name", Schema[String], get0 = _.name, set0 = (x, v) => x.copy(name = v)),
      Field("age", Schema[Int], get0 = _.age, set0 = (x, v) => x.copy(age = v)),
      Person.apply
    )

  implicit val userLoginSchema: Schema[UserLogin] =
    Schema.CaseClass2[String, String, UserLogin](
      TypeId.parse(classOf[UserLogin].getName),
      Field("username", Schema[String], get0 = _.username, set0 = (x, v) => x.copy(username = v)),
      Field("password", Schema[String], get0 = _.password, set0 = (x, v) => x.copy(password = v)),
      UserLogin.apply
    )

  implicit val activeUser: Schema[ActiveUser] =
    Schema.CaseClass3[Person, UserLogin, Boolean, ActiveUser](
      TypeId.parse(classOf[ActiveUser].getName),
      Field("person", Schema[Person], get0 = _.person, set0 = (x, v) => x.copy(person = v)),
      Field("login", Schema[UserLogin], get0 = _.login, set0 = (x, v) => x.copy(login = v)),
      Field("isActive", Schema[Boolean], get0 = _.isActive, set0 = (x, v) => x.copy(isActive = v)),
      ActiveUser.apply
    )

  implicit val transaction: Schema[Transfer] =
    Schema.CaseClass3[Long, Double, Option[String], Transfer](
      TypeId.parse(classOf[Transfer].getName),
      Field("id", Schema[Long], get0 = _.id, set0 = (x, v) => x.copy(id = v)),
      Field("amount", Schema[Double], get0 = _.amount, set0 = (x, v) => x.copy(amount = v)),
      Field("location", Schema[Option[String]], get0 = _.location, set0 = (x, v) => x.copy(location = v)),
      Transfer.apply
    )

  implicit val personEncoder: JdbcEncoder[Person]         = JdbcEncoder.fromSchema[Person]
  implicit val userLoginEncoder: JdbcEncoder[UserLogin]   = JdbcEncoder.fromSchema[UserLogin]
  implicit val activeUserEncoder: JdbcEncoder[ActiveUser] = JdbcEncoder.fromSchema[ActiveUser]
  implicit val transactionEncoder: JdbcEncoder[Transfer]  = JdbcEncoder.fromSchema[Transfer]
}
