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

import java.sql.{ PreparedStatement, Types }
import scala.language.implicitConversions

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
    final case class Syntax(value: String)                  extends Segment
    final case class Param(value: Any, setter: Setter[Any]) extends Segment
    final case class Nested(sql: Sql[_])                    extends Segment

    implicit def paramSegment[A](a: A)(implicit setter: Sql.Setter[A]): Segment.Param =
      Segment.Param(a, setter.asInstanceOf[Sql.Setter[Any]])

    implicit def nestedSqlSegment[A](sql: Sql[A]): Segment.Nested = Segment.Nested(sql)
  }

  trait Setter[A] { self =>
    def setValue(ps: PreparedStatement, index: Int, value: A): Unit
    def setNull(ps: PreparedStatement, index: Int): Unit

    final def contramap[B](f: B => A): Setter[B] =
      Setter((ps, i, value) => self.setValue(ps, i, f(value)), (ps, i) => self.setNull(ps, i))
  }

  object Setter {
    def apply[A]()(implicit setter: Setter[A]): Setter[A] = setter

    def apply[A](onValue: (PreparedStatement, Int, A) => Unit, onNull: (PreparedStatement, Int) => Unit): Setter[A] =
      new Setter[A] {
        def setValue(ps: PreparedStatement, index: Int, value: A): Unit = onValue(ps, index, value)
        def setNull(ps: PreparedStatement, index: Int): Unit            = onNull(ps, index)
      }

    def forSqlType[A](onValue: (PreparedStatement, Int, A) => Unit, sqlType: Int): Setter[A] = new Setter[A] {
      def setValue(ps: PreparedStatement, index: Int, value: A): Unit = onValue(ps, index, value)
      def setNull(ps: PreparedStatement, index: Int): Unit            = ps.setNull(index, sqlType)
    }

    def other[A](onValue: (PreparedStatement, Int, A) => Unit, sqlType: String): Setter[A] = new Setter[A] {
      def setValue(ps: PreparedStatement, index: Int, value: A): Unit = onValue(ps, index, value)
      def setNull(ps: PreparedStatement, index: Int): Unit            = ps.setNull(index, Types.OTHER, sqlType)
    }

    implicit def optionParamSetter[A](implicit setter: Setter[A]): Setter[Option[A]] =
      Setter(
        (ps, i, value) =>
          value match {
            case Some(value) => setter.setValue(ps, i, value)
            case None        => setter.setNull(ps, i)
          },
        (ps, i) => setter.setNull(ps, i)
      )

    implicit val intSetter: Setter[Int]               = forSqlType((ps, i, value) => ps.setInt(i, value), Types.INTEGER)
    implicit val longSetter: Setter[Long]             = forSqlType((ps, i, value) => ps.setLong(i, value), Types.BIGINT)
    implicit val doubleSetter: Setter[Double]         = forSqlType((ps, i, value) => ps.setDouble(i, value), Types.DOUBLE)
    implicit val stringSetter: Setter[String]         = forSqlType((ps, i, value) => ps.setString(i, value), Types.VARCHAR)
    implicit val booleanSetter: Setter[Boolean]       = forSqlType((ps, i, value) => ps.setBoolean(i, value), Types.BOOLEAN)
    implicit val shortSetter: Setter[Short]           = forSqlType((ps, i, value) => ps.setShort(i, value), Types.SMALLINT)
    implicit val floatSetter: Setter[Float]           = forSqlType((ps, i, value) => ps.setFloat(i, value), Types.FLOAT)
    implicit val byteSetter: Setter[Byte]             = forSqlType((ps, i, value) => ps.setByte(i, value), Types.TINYINT)
    implicit val byteArraySetter: Setter[Array[Byte]] = forSqlType((ps, i, value) => ps.setBytes(i, value), Types.ARRAY)
    implicit val blobSetter: Setter[java.sql.Blob]    = forSqlType((ps, i, value) => ps.setBlob(i, value), Types.BLOB)
    implicit val sqlDateSetter: Setter[java.sql.Date] = forSqlType((ps, i, value) => ps.setDate(i, value), Types.DATE)
    implicit val sqlTimeSetter: Setter[java.sql.Time] = forSqlType((ps, i, value) => ps.setTime(i, value), Types.TIME)

    implicit val bigDecimalSetter: Setter[java.math.BigDecimal] =
      forSqlType((ps, i, value) => ps.setBigDecimal(i, value), Types.NUMERIC)
    implicit val sqlTimestampSetter: Setter[java.sql.Timestamp] =
      forSqlType((ps, i, value) => ps.setTimestamp(i, value), Types.TIMESTAMP)

    implicit val uuidParamSetter: Setter[java.util.UUID] = other((ps, i, value) => ps.setObject(i, value), "uuid")

    implicit val charSetter: Setter[Char]                             = stringSetter.contramap(_.toString)
    implicit val bigIntSetter: Setter[java.math.BigInteger]           = bigDecimalSetter.contramap(new java.math.BigDecimal(_))
    implicit val bigDecimalScalaSetter: Setter[scala.math.BigDecimal] = bigDecimalSetter.contramap(_.bigDecimal)
    implicit val byteChunkSetter: Setter[Chunk[Byte]]                 = byteArraySetter.contramap(_.toArray)
    implicit val instantSetter: Setter[java.time.Instant]             = sqlTimestampSetter.contramap(java.sql.Timestamp.from)
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
