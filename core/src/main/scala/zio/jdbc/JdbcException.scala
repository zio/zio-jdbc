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

import zio._

import java.io.IOException
import java.sql.{ Connection, ResultSetMetaData, SQLException }

/**
 * Trait to encapsule all the exceptions employed by ZIO JDBC
 */
sealed trait JdbcException extends Exception

sealed trait ConnectionException extends JdbcException
sealed trait QueryException      extends JdbcException

/**
 * ConnectionException. Related to the connection operations with a database
 */
final case class DriverNotFound(cause: Throwable, driver: String)
    extends Exception(s"Could not found driver: $driver", cause)
    with ConnectionException
final case class FailedToConnect(cause: Throwable)        extends Exception(cause) with ConnectionException
final case class FailedMakingRestorable(cause: Throwable) extends Exception(cause) with ConnectionException

/**
 * QueryExceptions. Related to transactions
 */
sealed trait CodecException extends QueryException
sealed trait FailedQuery    extends QueryException

/**
 * CodecExceptions. Related to the decoding and encoding of the data in a transaction
 */
final case class DecodeException(cause: Throwable) extends Exception(cause) with CodecException
final case class JdbcDecoderError(
  message: String,
  cause: Throwable,
  metadata: ResultSetMetaData,
  row: Int,
  column: Option[Int] = None
) extends IOException(message, cause)
    with CodecException
final case class JdbcEncoderError(message: String, cause: Throwable)
    extends IOException(message, cause)
    with CodecException

/**
 * FailedQueries. Related to the failure of actions executed directly on a database
 */
final case class ZSQLException(cause: SQLException) extends Exception(cause) with FailedQuery
final case class FailedAccess[+A](cause: Throwable, f: Connection => ZIO[Scope, Throwable, A])
    extends Exception(cause)
    with FailedQuery
