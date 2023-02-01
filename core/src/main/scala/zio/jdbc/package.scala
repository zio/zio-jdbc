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
package zio

import zio.stream._

import scala.language.implicitConversions

package object jdbc {

  /**
   * A special purpose type Alias representing a sql fragment that is not yet fully formed,nor mapped to concrete data type
   */
  type SqlFragment = Sql[ZResultSet]

  implicit def sqlInterpolator(sc: StringContext): SqlInterpolator = new SqlInterpolator(sc)

  /**
   * Converts a String into a pure SQL expression
   */
  implicit def stringToSql(s: String): SqlFragment = Sql(Chunk(Sql.Segment.Syntax(s)), identity)

  /**
   * Executes a SQL delete query.
   */
  def delete(sql: SqlFragment): ZIO[ZConnection, Throwable, Long] =
    for {
      connection <- ZIO.service[ZConnection]
      result <- ZIO.scoped(connection.executeSqlWith(sql)(_.executeLargeUpdate()))
    } yield result

  /**
   * Executes a SQL statement, such as one that creates a table.
   */
  def execute(sql: SqlFragment): ZIO[ZConnection, Throwable, Unit] =
    for {
      connection <- ZIO.service[ZConnection]
      _ <- ZIO.scoped(connection.executeSqlWith(sql)(_.executeUpdate()))
    } yield ()

  /**
   * Performs an SQL insert query, returning a count of rows inserted and a
   * [[zio.Chunk]] of auto-generated keys. By default, auto-generated keys are
   * parsed and returned as `Chunk[Long]`. If keys are non-numeric, a
   * `Chunk.empty` is returned.
   */
  def insert(sql: SqlFragment): ZIO[ZConnection, Throwable, UpdateResult] =
    ZIO.scoped {
      for {
        connection <- ZIO.service[ZConnection]
        result <- connection.executeSqlWith(sql) { ps =>
          val rowsUpdated = ps.executeLargeUpdate()
          val updatedKeys = ps.getGeneratedKeys()
          ZIO.addFinalizer(ZIO.attemptBlocking(updatedKeys.close()).ignore)
          (rowsUpdated, updatedKeys)
        }
        (count, keysRs) = result
        chunk <- ZIO.attempt {
          val builder = ChunkBuilder.make[Long]()
          while (keysRs.next())
            builder += keysRs.getLong(1)
          builder.result()
        }.orElseSucceed(Chunk.empty)
      } yield UpdateResult(count, chunk)
    }

  /**
   * Performs a SQL select query, returning all results in a chunk.
   */
  def selectAll[A](sql: Sql[A]): ZIO[ZConnection, Throwable, Chunk[A]] =
    ZIO.scoped {
      for {
        zrs <- withResultSet(sql)
        chunk <- ZIO.attempt {
          val builder = ChunkBuilder.make[A]()
          while (zrs.resultSet.next())
            builder += sql.decode(zrs)
          builder.result()
        }
      } yield chunk
    }

  /**
   * Performs a SQL select query, returning the first result, if any.
   */
  def selectOne[A](sql: Sql[A]): ZIO[ZConnection, Throwable, Option[A]] =
    ZIO.scoped {
      for {
        zrs <- withResultSet(sql)
        option <- if (zrs.resultSet.next()) ZIO.some(sql.decode(zrs)) else ZIO.none
      } yield option
    }

  /**
   * Performs a SQL select query, returning a stream of results.
   */
  def selectStream[A](sql: Sql[A]): ZStream[ZConnection, Throwable, A] =
    ZStream.unwrapScoped {
      for {
        zrs <- withResultSet(sql)
      } yield ZStream.repeatZIOOption {
        ZIO.suspend {
          for {
            hasNext <- zrs.access(_.next())
            maybeValue <- if (hasNext) ZIO.attempt(Some(sql.decode(zrs))) else ZIO.none
          } yield maybeValue
        }
          .mapError(Option(_))
          .flatMap {
            case None => ZIO.fail(None)
            case Some(v) => ZIO.succeed(v)
          }
      }
    }

  /**
   * A new transaction, which may be applied to ZIO effects that require a
   * connection in order to execute such effects in the transaction.
   */
  val transaction: ZLayer[ZConnectionPool, Throwable, ZConnection] =
    ZLayer(ZIO.serviceWith[ZConnectionPool](_.transaction)).flatten

  /**
   * Performs a SQL update query, returning a count of rows updated.
   */
  def update(sql: SqlFragment): ZIO[ZConnection, Throwable, Long] =
    for {
      connection <- ZIO.service[ZConnection]
      result <- ZIO.scoped(connection.executeSqlWith(sql)(_.executeLargeUpdate()))
    } yield result

  private def withResultSet[A](sql: Sql[A]): ZIO[ZConnection with Scope, Throwable, ZResultSet] =
    for {
      connection <- ZIO.service[ZConnection]
      zrs <- ZIO.acquireRelease(connection.executeSqlWith(sql)(_.executeQuery()).map(ZResultSet.apply))(_.access(_.close()).ignore)
    } yield zrs

}
