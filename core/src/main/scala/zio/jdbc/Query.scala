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
import zio.stream._

import java.sql.SQLException

final case class Query[+A](decode: ZResultSet => IO[CodecException, A], sql: SqlFragment) {

  def as[B](implicit decoder: JdbcDecoder[B]): Query[B] =
    Query(zrs => decoder.decode(1, zrs.resultSet).map(_._2), sql)

  def map[B](f: A => B): Query[B] =
    Query(zrs => decode(zrs).map(f), sql)

  /**
   * Performs a SQL select query, returning all results in a chunk.
   */
  def selectAll: ZIO[ZConnection, QueryException, Chunk[A]] =
    ZIO.scoped(for {
      zrs   <- executeQuery(sql)
      chunk <- ZIO.iterate(ChunkBuilder.make[A]())(_ => zrs.next()) { builder =>
                 for {
                   decoded <- decode(zrs)
                 } yield builder += decoded
               }
    } yield chunk.result())

  /**
   * Performs a SQL select query, returning the first result, if any.
   */
  def selectOne: ZIO[ZConnection, QueryException, Option[A]] =
    ZIO.scoped(for {
      zrs    <- executeQuery(sql)
      option <-
        if (zrs.next()) decode(zrs).map(Some(_))
        else ZIO.none
    } yield option)

  /**
   * Performs a SQL select query, returning a stream of results.
   */
  def selectStream: ZStream[ZConnection, QueryException, A] =
    ZStream.unwrapScoped {
      for {
        zrs   <- executeQuery(sql)
        stream = ZStream.repeatZIOOption {
                   ZIO
                     .suspendSucceed(if (zrs.next()) decode(zrs).map(Some(_)) else ZIO.none)
                     .mapError(Some(_))
                     .flatMap {
                       case None    => ZIO.fail(None)
                       case Some(v) => ZIO.succeed(v)
                     }
                 }
      } yield stream
    }

  def withDecode[B](f: ZResultSet => B): Query[B] =
    Query(sql, f)

  private def executeQuery(sql: SqlFragment): ZIO[Scope with ZConnection, ZSQLException, ZResultSet] = for {
    connection <- ZIO.service[ZConnection]
    zrs        <- connection.executeSqlWith(sql) { ps =>
                    ZIO.acquireRelease {
                      ZIO.attempt(ZResultSet(ps.executeQuery())).refineOrDie { case e: SQLException =>
                        ZSQLException(e)
                      }
                    }(_.close)
                  }
  } yield zrs

}

object Query {

  def apply[A](sql: SqlFragment, decode: ZResultSet => A): Query[A] = {
    def decodeZIO(zrs: ZResultSet): IO[DecodeException, A] =
      ZIO.attempt(decode(zrs)).refineOrDie { case e: Throwable =>
        DecodeException(e)
      }
    new Query[A](zrs => decodeZIO(zrs), sql)
  }

  def fromSqlFragment[A](sql: SqlFragment)(implicit decoder: JdbcDecoder[A]): Query[A] =
    Query[A](sql, (zrs: ZResultSet) => decoder.unsafeDecode(1, zrs.resultSet)._2)

}
