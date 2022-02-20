package zio.jdbc

import java.sql.SQLException
import scala.util.control.NoStackTrace

final case class ZioJdbcException(cause: Throwable, sql: Sql[_]) extends SQLException(cause) with NoStackTrace {
  override def getMessage: String =
    s"""
       |   error: ${super.getMessage}  
       |   sql: ${sql.toString()}""".stripMargin
}
