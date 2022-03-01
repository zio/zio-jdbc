package zio.jdbc

import java.io.IOException

final case class JdbcEncoderError(message: String, cause: Throwable) extends IOException(message, cause)
