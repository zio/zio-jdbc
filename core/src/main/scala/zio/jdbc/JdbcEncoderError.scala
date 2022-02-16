package zio.jdbc

import java.io.IOException

case class JdbcEncoderError(message: String, cause: Throwable) extends IOException(message, cause)
