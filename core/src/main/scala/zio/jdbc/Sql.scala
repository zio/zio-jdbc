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

import zio.jdbc.Sql.Segment
import zio.{ Chunk, ChunkBuilder }

import java.sql.PreparedStatement

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

  def ++(that: SqlFragment)(implicit ev: IsSqlFragment[A]): SqlFragment =
    new Sql(builder => { self.build(builder); that.build(builder) }, that.decode)

  def and(first: SqlFragment, rest: SqlFragment*)(implicit ev: IsSqlFragment[A]): SqlFragment =
    and(first +: rest)

  def and(elements: Iterable[SqlFragment])(implicit ev: IsSqlFragment[A]): SqlFragment =
    self ++ Sql.prependEach(Sql.and, elements)

  def as[B](implicit decode: JdbcDecoder[B]): Sql[B] =
    new Sql(build, (rs: ZResultSet) => decode.unsafeDecode(rs.resultSet))

  override def equals(that: Any): Boolean =
    that match {
      case that: Sql[_] => (self.segments, self.decode) == ((that.segments, that.decode))

      case _ => false
    }

  def from(table: SqlFragment)(implicit ev: IsSqlFragment[A]): SqlFragment =
    self ++ Sql.from ++ table

  override def hashCode: Int = (segments, decode).hashCode

  def in[B](b: B, bs: B*)(implicit encode: JdbcEncoder[B], ev: IsSqlFragment[A]): SqlFragment =
    in(b +: bs)

  def in[B](bs: Iterable[B])(implicit encode: JdbcEncoder[B], ev: IsSqlFragment[A]): SqlFragment =
    in0(Sql.in, bs)

  def map[B](f: A => B): Sql[B] =
    new Sql(build, rs => f(decode(rs)))

  def not(fragment: SqlFragment)(implicit ev: IsSqlFragment[A]): SqlFragment =
    self ++ Sql.not ++ fragment

  def notIn[B](b: B, bs: B*)(implicit encode: JdbcEncoder[B], ev: IsSqlFragment[A]): SqlFragment =
    notIn(b +: bs)

  def notIn[B](bs: Iterable[B])(implicit encode: JdbcEncoder[B], ev: IsSqlFragment[A]): SqlFragment =
    in0(Sql.notIn, bs)

  def or(first: SqlFragment, rest: SqlFragment*)(implicit ev: IsSqlFragment[A]): SqlFragment =
    or(first +: rest)

  def or(elements: Iterable[SqlFragment])(implicit ev: IsSqlFragment[A]): SqlFragment =
    self ++ Sql.prependEach(Sql.or, elements)

  def segments: Chunk[Sql.Segment] = {
    val builder = ChunkBuilder.make[Sql.Segment]()
    build(builder)
    builder.result()
  }

  private[jdbc] def foreachSegment(addSyntax: Segment.Syntax => Any)(addParam: Segment.Param => Any): Unit =
    segments.foreach {
      case syntax: Segment.Syntax => addSyntax(syntax)
      case param: Segment.Param   => addParam(param)
      case nested: Segment.Nested => nested.sql.foreachSegment(addSyntax)(addParam)
    }

  override def toString: String = {
    val sql           = new StringBuilder()
    val paramsBuilder = ChunkBuilder.make[String]()

    foreachSegment { syntax =>
      sql.append(syntax.value)
    } { param =>
      sql.append("?")
      paramsBuilder += param.value.toString
    }

    val params       = paramsBuilder.result()
    val paramsString = if (params.isEmpty) "" else ", " + params.mkString(", ")

    s"Sql(${sql.result()}$paramsString)"
  }

  def values[B](bs: Iterable[B])(implicit encode: JdbcEncoder[B], ev: IsSqlFragment[A]): SqlFragment =
    this ++
      Sql.values ++
      Sql.intersperse(
        Sql.comma,
        bs.map(b => Sql.lparen ++ encode.encode(b) ++ Sql.rparen)
      )

  def values[B](b: B, bs: B*)(implicit encoder: JdbcEncoder[B], ev: IsSqlFragment[A]): SqlFragment = values(b +: bs)

  def where(predicate: SqlFragment)(implicit ev: IsSqlFragment[A]): SqlFragment =
    self ++ Sql.where ++ predicate

  def withDecode[B](f: ZResultSet => B): Sql[B] =
    Sql(segments, f)

  private def in0[B](op: SqlFragment, bs: Iterable[B])(implicit
    encode: JdbcEncoder[B],
    ev: IsSqlFragment[A]
  ): SqlFragment =
    self ++ op ++ Sql.lparen ++ Sql.intersperse(Sql.comma, bs.map(encode.encode)) ++ Sql.rparen
}

object Sql {
  val empty: SqlFragment = Sql(Chunk.empty, identity)

  sealed trait Segment
  object Segment {
    final case class Syntax(value: String)                       extends Segment
    final case class Param(value: Any, setter: ParamSetter[Any]) extends Segment
    final case class Nested(sql: Sql[_])                         extends Segment
  }

  trait ParamSetter[A] { self =>
    def apply(preparedStatement: PreparedStatement, index: Int, value: A): Unit

    final def contramap[B](f: B => A): ParamSetter[B] = (ps, i, bValue) => self(ps, i, f(bValue))
  }

  object ParamSetter {
    implicit val intParamSetter: ParamSetter[Int]                               = (ps, i, value) => ps.setInt(i, value)
    implicit val longParamSetter: ParamSetter[Long]                             = (ps, i, value) => ps.setLong(i, value)
    implicit val doubleParamSetter: ParamSetter[Double]                         = (ps, i, value) => ps.setDouble(i, value)
    implicit val stringParamSetter: ParamSetter[String]                         = (ps, i, value) => ps.setString(i, value)
    implicit val charParamSetter: ParamSetter[Char]                             = stringParamSetter.contramap(_.toString)
    implicit val booleanParamSetter: ParamSetter[Boolean]                       = (ps, i, value) => ps.setBoolean(i, value)
    implicit val bigDecimalParamSetter: ParamSetter[java.math.BigDecimal]       = (ps, i, value) => ps.setBigDecimal(i, value)
    implicit val bigIntParamSetter: ParamSetter[java.math.BigInteger]           =
      bigDecimalParamSetter.contramap(new java.math.BigDecimal(_))
    implicit val bigDecimalScalaParamSetter: ParamSetter[scala.math.BigDecimal] =
      bigDecimalParamSetter.contramap(_.bigDecimal)
    implicit val shortParamSetter: ParamSetter[Short]                           = (ps, i, value) => ps.setShort(i, value)
    implicit val floatParamSetter: ParamSetter[Float]                           = (ps, i, value) => ps.setFloat(i, value)
    implicit val byteParamSetter: ParamSetter[Byte]                             = (ps, i, value) => ps.setByte(i, value)
    implicit val byteArrayParamSetter: ParamSetter[Array[Byte]]                 = (ps, i, value) => ps.setBytes(i, value)
    implicit val byteChunkParamSetter: ParamSetter[Chunk[Byte]]                 = byteArrayParamSetter.contramap(_.toArray)
    implicit val blobParamSetter: ParamSetter[java.sql.Blob]                    = (ps, i, value) => ps.setBlob(i, value)
    implicit val sqlDateParamSetter: ParamSetter[java.sql.Date]                 = (ps, i, value) => ps.setDate(i, value)
    implicit val sqlTimeParamSetter: ParamSetter[java.sql.Time]                 = (ps, i, value) => ps.setTime(i, value)
    implicit val sqlTimestampParamSetter: ParamSetter[java.sql.Timestamp]       = (ps, i, value) => ps.setTimestamp(i, value)
    implicit val instantParamSetter: ParamSetter[java.time.Instant]             =
      sqlTimestampParamSetter.contramap(java.sql.Timestamp.from)
    implicit val uuidParamSetter: ParamSetter[java.util.UUID]                   = (ps, i, value) => ps.setObject(i, value)
  }

  def apply(sql: String): Sql[ZResultSet] = sql

  def apply[A](segments: Chunk[Sql.Segment], decode: ZResultSet => A): Sql[A] =
    new Sql(builder => builder ++= segments, decode)

  def deleteFrom(table: String): SqlFragment =
    s"DELETE FROM $table"

  def insertInto(table: String)(keys: String*): SqlFragment =
    s"INSERT INTO $table (${keys.mkString(", ")})"

  def select(columns: String*): SqlFragment =
    s"SELECT ${columns.mkString(", ")}"

  def update(table: String): SqlFragment =
    s"UPDATE $table"

  private[jdbc] def intersperse(
    sep: SqlFragment,
    elements: Iterable[SqlFragment]
  ): SqlFragment = {

    var first = true

    elements.foldLeft(empty) { (acc, element) =>
      if (!first) acc ++ sep ++ element
      else {
        first = false
        acc ++ element
      }
    }
  }

  private[jdbc] def prependEach(
    sep: SqlFragment,
    elements: Iterable[SqlFragment]
  ): SqlFragment =
    elements.foldLeft(empty) { (acc, element) =>
      acc ++ sep ++ element
    }

  private[jdbc] val and                                  = sql" AND "
  private[jdbc] val comma                                = sql","
  private[jdbc] val from                                 = sql" FROM "
  private[jdbc] val identityFn: ZResultSet => ZResultSet = a => a
  private[jdbc] val in                                   = sql" IN "
  private[jdbc] val lparen                               = sql"("
  private[jdbc] val not                                  = sql" NOT "
  private[jdbc] val notIn                                = sql" NOT IN "
  private[jdbc] val nullLiteral                          = sql"NULL"
  private[jdbc] val or                                   = sql" OR "
  private[jdbc] val rparen                               = sql")"
  private[jdbc] val values                               = sql" VALUES "
  private[jdbc] val where                                = sql" WHERE "
}
