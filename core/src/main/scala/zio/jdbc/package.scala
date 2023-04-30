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

  implicit def sqlInterpolator(sc: StringContext): SqlInterpolator = new SqlInterpolator(sc)

  /**
   * Converts a String into a pure SQL expression
   */
  implicit def stringToSql0(s: String): SqlFragment = SqlFragment(Chunk(SqlFragment.Segment.Syntax(s)))

  /**
   * Executes a SQL delete query.
   */
//  def delete(sql: SqlFragment): ZIO[ZConnection, Throwable, Long] =
//    ZIO.scoped(executeLargeUpdate(sql))

  /**
   * Performs an SQL insert query, returning a count of rows inserted and a
   * [[zio.Chunk]] of auto-generated keys. By default, auto-generated keys are
   * parsed and returned as `Chunk[Long]`. If keys are non-numeric, a
   * `Chunk.empty` is returned.
   */
//  def insert(sql: SqlFragment): ZIO[ZConnection, Throwable, UpdateResult] =
//    ZIO.scoped(executeWithUpdateResult(sql))

  /**
   * A new transaction, which may be applied to ZIO effects that require a
   * connection in order to execute such effects in the transaction.
   */
  val transaction: ZLayer[ZConnectionPool, Throwable, ZConnection] =
    ZLayer(ZIO.serviceWith[ZConnectionPool](_.transaction)).flatten

  /**
   * Performs a SQL update query, returning a count of rows updated.
   */
//  def update(sql: SqlFragment): ZIO[ZConnection, Throwable, Long] =
//    ZIO.scoped(executeLargeUpdate(sql))

//  private def executeLargeUpdate[A](sql: Sql[A]) = for {
//    connection <- ZIO.service[ZConnection]
//    count      <- connection.executeSqlWith(sql) { ps =>
//                    ZIO.attempt(ps.executeLargeUpdate())
//                  }
//  } yield count

//  private def executeWithUpdateResult[A](sql: Sql[A]) = for {
//    connection <- ZIO.service[ZConnection]
//    result     <- connection.executeSqlWith(sql) { ps =>
//                    for {
//                      result     <- ZIO.acquireRelease(ZIO.attempt {
//                                      val rowsUpdated = ps.executeLargeUpdate()
//                                      val updatedKeys = ps.getGeneratedKeys
//                                      (rowsUpdated, ZResultSet(updatedKeys))
//                                    })(_._2.close)
//                      (count, rs) = result
//                      keys       <- ZIO.attempt {
//                                      val builder = ChunkBuilder.make[Long]()
//                                      while (rs.next())
//                                        builder += rs.resultSet.getLong(1)
//                                      builder.result()
//                                    }.orElseSucceed(Chunk.empty)
//
//                    } yield UpdateResult(count, keys)
//                  }
//  } yield result

}
