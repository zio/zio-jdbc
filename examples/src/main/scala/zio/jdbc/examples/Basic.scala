package zio.jdbc.examples

import zio.jdbc._

object Basic {
  val ex1 = sql"select * from users"

  val ex2 = sql"select name, age from users".as[(String, Int)]
}
