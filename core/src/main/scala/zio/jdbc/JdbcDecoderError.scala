package zio.jdbc

import java.io.IOException
import java.sql.ResultSetMetaData

final case class JdbcDecoderError(
  message: String,
  cause: Throwable,
  metadata: ResultSetMetaData,
  row: Int,
  column: Option[Int] = None
) extends IOException(message, cause)
