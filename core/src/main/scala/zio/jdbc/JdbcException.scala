/*
 * Copyright 2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package zio.jdbc

import java.io.IOException
import java.sql.{ ResultSetMetaData, SQLException, SQLTimeoutException }

/**
 * Trait to encapsule all the exceptions employed by ZIO JDBC
 */
sealed trait JdbcException extends Exception

/**
 * Exceptions subtraits to specify the type of error
 */
sealed trait ConnectionException extends JdbcException
sealed trait QueryException      extends JdbcException
sealed trait CodecException      extends JdbcException with QueryException

sealed trait FatalException       extends JdbcException
sealed trait RecoverableException extends JdbcException

// ZTimeoutException groups all the errors produced by SQLTimeoutException.
sealed trait ZTimeoutException extends JdbcException

/**
 * ConnectionException. Related to the connection operations with a database
 */
final case class DriverNotFound(cause: Throwable, driver: String)
    extends Exception(s"Could not found driver: $driver", cause)
    with ConnectionException
    with FatalException
final case class DBError(cause: Throwable) extends Exception(cause) with ConnectionException with FatalException
final case class FailedMakingRestorable(cause: Throwable)
    extends Exception(cause)
    with ConnectionException
    with FatalException
final case class ConnectionTimeout(cause: Throwable)
    extends Exception(cause)
    with ConnectionException
    with ZTimeoutException
    with RecoverableException

/**
 * CodecExceptions. Related to the decoding and encoding of the data in a transaction
 */
final case class DecodeException(cause: Throwable) extends Exception(cause) with CodecException with FatalException
final case class JdbcDecoderError(
  message: String,
  cause: Throwable,
  metadata: ResultSetMetaData,
  row: Int,
  column: Option[Int] = None
) extends IOException(message, cause)
    with CodecException
    with FatalException
final case class JdbcEncoderError(message: String, cause: Throwable)
    extends IOException(message, cause)
    with CodecException
    with FatalException

/**
 * FailedQueries. Related to the failure of actions executed directly on a database
 */
final case class ZSQLException(cause: SQLException)
    extends Exception(cause)
    with QueryException
    with RecoverableException
final case class ZSQLTimeoutException(cause: SQLTimeoutException)
    extends Exception(cause)
    with QueryException
    with ZTimeoutException
    with RecoverableException
