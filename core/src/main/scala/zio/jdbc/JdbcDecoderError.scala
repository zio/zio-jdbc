package zio.jdbc

import java.sql.ResultSetMetaData
import java.io.IOException

final case class JdbcDecoderError(
  message: String,
  cause: Throwable,
  metadata: ResultSetMetaData,
  row: Int,
  column: Option[Int] = None
) extends IOException(message, cause)
