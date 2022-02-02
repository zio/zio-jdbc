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

import zio.{ Chunk, ChunkBuilder }

/**
 * A `Sql[A]` represents part or all of a SQL query, together with an
 * optional decoder that can produce an `A` from a `ResultSet`. The SQL
 * is described by a sequence of segments, each segment being either a
 * fragment of SQL, or a value to be inserted into the query in a way that
 * is safe from SQL injection attacks.
 *
 * @param segments
 * @param decode
 */
final class Sql[+A](
  private[jdbc] val build: ChunkBuilder[Sql.Segment] => Unit,
  val decode: ZResultSet => A
) { self =>
  def +(that: Sql[ZResultSet])(implicit ev: A <:< ZResultSet): Sql[ZResultSet] =
    new Sql(builder => { self.build(builder); that.build(builder) }, that.decode)

  def as[B](implicit decode: JdbcDecoder[B]): Sql[B] =
    new Sql(build, (rs: ZResultSet) => decode.unsafeDecode(rs.resultSet))

  override def equals(that: Any): Boolean =
    that match {
      case that: Sql[_] => (self.segments, self.decode) == ((that.segments, that.decode))

      case _ => false
    }

  override def hashCode: Int = (segments, decode).hashCode

  def map[B](f: A => B): Sql[B] =
    new Sql(build, rs => f(decode(rs)))

  def segments: Chunk[Sql.Segment] = {
    val builder = ChunkBuilder.make[Sql.Segment]()
    build(builder)
    builder.result()
  }

  def values[B](
    iterable: Iterable[B]
  )(implicit encode: JdbcEncoder[B], ev: A <:< ZResultSet): Sql[ZResultSet] =
    Sql.values +
      Sql.intersperse(
        Sql.comma,
        iterable.map(b => Sql.lparen + encode.encode(b) + Sql.rparen)
      )

  def values[B](
    bs: B*
  )(implicit encode: JdbcEncoder[B], ev: A <:< ZResultSet): Sql[ZResultSet] = values(bs.toIterable)

  def withDecode[B](f: ZResultSet => B): Sql[B] =
    Sql(segments, f)
}
object Sql {
  val empty: Sql[ZResultSet] = Sql(Chunk.empty, identity(_))

  def apply[A](segments: Chunk[Sql.Segment], decode: ZResultSet => A): Sql[A] =
    new Sql(builder => builder ++= segments, decode)

  sealed trait Segment
  object Segment {
    final case class Syntax(value: String) extends Segment
    final case class Param(value: Any)     extends Segment
  }

  private[jdbc] def intersperse(
    sep: Sql[ZResultSet],
    elements: Iterable[Sql[ZResultSet]]
  ): Sql[ZResultSet] = {

    var first = true

    elements.foldLeft(empty) { (acc, element) =>
      if (!first) acc + sep + element
      else {
        first = false
        acc + element
      }
    }
  }

  private[jdbc] val identityFn: ZResultSet => ZResultSet = a => a
  private[jdbc] val values                               = sql""" VALUES """
  private[jdbc] val lparen                               = sql"""("""
  private[jdbc] val rparen                               = sql""")"""
  private[jdbc] val comma                                = sql""","""
  private[jdbc] val nullLiteral                          = sql"""NULL"""
}
