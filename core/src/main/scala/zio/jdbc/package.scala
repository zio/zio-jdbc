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

import zio.jdbc.SqlFragment.{ Segment, Setter }
import zio.jdbc.{ JdbcEncoder, SqlFragment }

import scala.language.implicitConversions

package object jdbc extends LowPriorityImplicits1 {

  implicit def sqlInterpolator(sc: StringContext): SqlInterpolator = new SqlInterpolator(sc)

  /**
   * Converts a String into a pure SQL expression
   */
  implicit def stringToSql(s: String): SqlFragment = SqlFragment(Chunk(SqlFragment.Segment.Syntax(s)))

  /**
   * A new transaction, which may be applied to ZIO effects that require a
   * connection in order to execute such effects in the transaction.
   */
  val transaction: ZLayer[ZConnectionPool, Throwable, ZConnection] =
    ZLayer(ZIO.serviceWith[ZConnectionPool](_.transaction)).flatten

}
trait LowPriorityImplicits1 extends LowPriorityImplicits2 {

  implicit def paramSegment[A](a: A)(implicit setter: Setter[A]): Segment.Param =
    Segment.Param(a, setter.asInstanceOf[Setter[Any]])

  implicit def nestedSqlSegment[A](sql: SqlFragment): Segment.Nested = Segment.Nested(sql)
}

trait LowPriorityImplicits2 {
  implicit def segmentFromJdbcEncoder[A](obj: A)(implicit encoder: JdbcEncoder[A]): Segment =
    Segment.Nested(encoder.encode(obj))
}
