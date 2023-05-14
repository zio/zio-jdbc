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

import zio.Chunk
import zio.jdbc.SqlFragment.{ Segment, values }
import zio.schema.{ Schema, StandardType }

import java.sql.{ PreparedStatement, Types }

/**
 * A type class that describes the ability to convert a value of type `A` into
 * a fragment of SQL. This is useful for forming SQL insert statements.
 */
trait JdbcEncoder[-A] { self =>
  def encode(value: A): SqlFragment

  /**
   * Returns first index after encoder
   */
  def unsafeSetValue(ps: PreparedStatement, index: Int, value: A): Int

  /**
   * Returns first index after encoder
   */
  def unsafeSetNull(ps: PreparedStatement, index: Int): Int

  final def contramap[B](f: B => A): JdbcEncoder[B] =
    new JdbcEncoder[B] {
      override def encode(value: B): SqlFragment =
        self.encode(f(value))

      override def unsafeSetValue(ps: PreparedStatement, index: Int, value: B): Int =
        self.unsafeSetValue(ps, index, f(value))

      override def unsafeSetNull(ps: PreparedStatement, index: Int): Int = self.unsafeSetNull(ps, index)

      override def sql(a: B): String = "?"
    }

  def sql(value: A): String
  def prettyValuePrinter(value: A): String = value.toString
}

object JdbcEncoder extends JdbcEncoder0LowPriorityImplicits {

  /**
   * Restrictions:
   * - Placeholder must contain only one "?"
   */
  def single[A](placeholder: String, valueToString: A => String): JdbcEncoder[A] = {
    val questionMarkCount = placeholder.count(_ == '?')
    if (questionMarkCount == 0) {
      throw new IllegalArgumentException("Placeholder must contain one '?'")
    }
    if (questionMarkCount > 1) {
      throw new IllegalArgumentException("Placeholder can contain only one '?'")
    }
    new JdbcEncoder[A] {

      override def encode(value: A): SqlFragment =
        SqlFragment(Chunk(SqlFragment.Segment.Syntax.apply(placeholder)))

      override def unsafeSetValue(ps: PreparedStatement, index: Int, value: A): Int = {
        ps.setString(index, valueToString(value))
        index + 1
      }

      override def unsafeSetNull(ps: PreparedStatement, index: Int): Int = {
        ps.setNull(index, Types.VARCHAR)
        index + 1
      }

      override def sql(value: A): String = placeholder

      override def prettyValuePrinter(value: A): String = valueToString(value)
    }
  }

  def apply[A]()(implicit encoder: JdbcEncoder[A]): JdbcEncoder[A] = encoder

  def apply[A](
    onEncode: A => SqlFragment,
    onSql: => String,
    onValue: (PreparedStatement, Int, A) => Unit,
    onNull: (PreparedStatement, Int) => Unit
  ): JdbcEncoder[A] =
    new JdbcEncoder[A] {

      override def encode(value: A): SqlFragment = onEncode(value)

      override def unsafeSetValue(ps: PreparedStatement, index: Int, value: A): Int = {
        onValue(ps, index, value)
        index + 1
      }

      override def unsafeSetNull(ps: PreparedStatement, index: Int): Int = {
        onNull(ps, index)
        index + 1
      }

      override def sql(a: A): String = onSql
    }

  private def forSqlType[A](onValue: (PreparedStatement, Int, A) => Unit, sqlType: Int): JdbcEncoder[A] =
    new JdbcEncoder[A] {

      override def encode(value: A): SqlFragment = SqlFragment(
        Chunk.apply(Segment.Param(value, this.asInstanceOf[JdbcEncoder[Any]]))
      )

      def unsafeSetValue(ps: PreparedStatement, index: Int, value: A): Int = {
        onValue(ps, index, value)
        index + 1
      }

      def unsafeSetNull(ps: PreparedStatement, index: Int): Int = {
        ps.setNull(index, sqlType)
        index + 1
      }

      override def sql(a: A): String = "?"
    }

  private def forIterableSqlType[A, I](
    iterator: I => Iterator[A],
    sqlType: Int
  )(implicit encoder: JdbcEncoder[A]): JdbcEncoder[I] =
    new JdbcEncoder[I] {

      override def encode(value: I): SqlFragment = SqlFragment(
        Chunk.apply(Segment.Param(value, this.asInstanceOf[JdbcEncoder[Any]]))
      )

      def unsafeSetValue(ps: PreparedStatement, index: Int, value: I): Int =
        iterator(value)
          .foldLeft(index) { case (i, value) => encoder.unsafeSetValue(ps, i, value) }

      def unsafeSetNull(ps: PreparedStatement, index: Int): Int = {
        ps.setNull(index, sqlType)
        index + 1
      }

      override def sql(a: I): String = iterator(a).map(_ => "?").mkString(",")

      override def prettyValuePrinter(value: I): String = iterator(value)
        .mkString(", ")
    }

  def other[A](onValue: (PreparedStatement, Int, A) => Unit, sqlType: String): JdbcEncoder[A] = new JdbcEncoder[A] {

    override def encode(value: A): SqlFragment = SqlFragment(
      Chunk.apply(Segment.Param(value, this.asInstanceOf[JdbcEncoder[Any]]))
    )

    def unsafeSetValue(ps: PreparedStatement, index: Int, value: A): Int = {
      onValue(ps, index, value)
      index + 1
    }

    def unsafeSetNull(ps: PreparedStatement, index: Int): Int = {
      ps.setNull(index, Types.OTHER, sqlType)
      index + 1
    }

    override def sql(a: A): String = "?"
  }

  implicit def optionParamEncoder[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[Option[A]] =
    JdbcEncoder(
      _.fold(SqlFragment.nullLiteral)(encoder.encode),
      "?",
      (ps, i, value) =>
        value match {
          case Some(value) => encoder.unsafeSetValue(ps, i, value)
          case None        => encoder.unsafeSetNull(ps, i)
        },
      (ps, i) => encoder.unsafeSetNull(ps, i)
    )

  implicit val intEncoder: JdbcEncoder[Int]               = forSqlType((ps, i, value) => ps.setInt(i, value), Types.INTEGER)
  implicit val longEncoder: JdbcEncoder[Long]             = forSqlType((ps, i, value) => ps.setLong(i, value), Types.BIGINT)
  implicit val doubleEncoder: JdbcEncoder[Double]         = forSqlType((ps, i, value) => ps.setDouble(i, value), Types.DOUBLE)
  implicit val stringEncoder: JdbcEncoder[String]         = forSqlType((ps, i, value) => ps.setString(i, value), Types.VARCHAR)
  implicit val booleanEncoder: JdbcEncoder[Boolean]       =
    forSqlType((ps, i, value) => ps.setBoolean(i, value), Types.BOOLEAN)
  implicit val shortEncoder: JdbcEncoder[Short]           = forSqlType((ps, i, value) => ps.setShort(i, value), Types.SMALLINT)
  implicit val floatEncoder: JdbcEncoder[Float]           = forSqlType((ps, i, value) => ps.setFloat(i, value), Types.FLOAT)
  implicit val byteEncoder: JdbcEncoder[Byte]             = forSqlType((ps, i, value) => ps.setByte(i, value), Types.TINYINT)
  implicit val byteArrayEncoder: JdbcEncoder[Array[Byte]] =
    forSqlType((ps, i, value) => ps.setBytes(i, value), Types.ARRAY)
  implicit val blobEncoder: JdbcEncoder[java.sql.Blob]    = forSqlType((ps, i, value) => ps.setBlob(i, value), Types.BLOB)
  implicit val sqlDateEncoder: JdbcEncoder[java.sql.Date] =
    forSqlType((ps, i, value) => ps.setDate(i, value), Types.DATE)
  implicit val sqlTimeEncoder: JdbcEncoder[java.sql.Time] =
    forSqlType((ps, i, value) => ps.setTime(i, value), Types.TIME)

  implicit def chunkEncoder[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[Chunk[A]] = iterableEncoder[A, Chunk[A]]

  implicit def listEncoder[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[List[A]] = iterableEncoder[A, List[A]]

  implicit def vectorEncoder[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[Vector[A]] =
    iterableEncoder[A, Vector[A]]

  implicit def setEncoder[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[Set[A]] = iterableEncoder[A, Set[A]]

  implicit def arrayEncoder[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[Array[A]] =
    forIterableSqlType(
      _.iterator,
      Types.OTHER
    )

  private def iterableEncoder[A, I <: Iterable[A]](implicit encoder: JdbcEncoder[A]): JdbcEncoder[I] =
    forIterableSqlType(
      _.iterator,
      Types.OTHER
    )

  implicit val bigDecimalEncoder: JdbcEncoder[java.math.BigDecimal] =
    forSqlType((ps, i, value) => ps.setBigDecimal(i, value), Types.NUMERIC)
  implicit val sqlTimestampEncoder: JdbcEncoder[java.sql.Timestamp] =
    forSqlType((ps, i, value) => ps.setTimestamp(i, value), Types.TIMESTAMP)

  implicit val uuidParamEncoder: JdbcEncoder[java.util.UUID] = other((ps, i, value) => ps.setObject(i, value), "uuid")

  implicit val charEncoder: JdbcEncoder[Char]                             = stringEncoder.contramap(_.toString)
  implicit val bigIntEncoder: JdbcEncoder[java.math.BigInteger]           =
    bigDecimalEncoder.contramap(new java.math.BigDecimal(_))
  implicit val bigDecimalScalaEncoder: JdbcEncoder[scala.math.BigDecimal] = bigDecimalEncoder.contramap(_.bigDecimal)
  implicit val byteChunkEncoder: JdbcEncoder[Chunk[Byte]]                 = byteArrayEncoder.contramap(_.toArray)
  implicit val instantEncoder: JdbcEncoder[java.time.Instant]             = sqlTimestampEncoder.contramap(java.sql.Timestamp.from)

  // TODO: review for cases like Option of a tuple
  implicit def tuple2Encoder[A: JdbcEncoder, B: JdbcEncoder]: JdbcEncoder[(A, B)] =
    tupleNEncoder(JdbcEncoder[A](), JdbcEncoder[B]())

  implicit def tuple3Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder]: JdbcEncoder[(A, B, C)] =
    tupleNEncoder(JdbcEncoder[A](), JdbcEncoder[B](), JdbcEncoder[C]())
  implicit def tuple4Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder, D: JdbcEncoder]
    : JdbcEncoder[(A, B, C, D)] =
    tupleNEncoder(JdbcEncoder[A](), JdbcEncoder[B](), JdbcEncoder[C](), JdbcEncoder[D]())

  private def tupleNEncoder[A <: Product](encoders: JdbcEncoder[_]*): JdbcEncoder[A] =
    new JdbcEncoder[A] {
      override def encode(value: A): SqlFragment =
        SqlFragment.intersperse(
          SqlFragment.comma,
          value.productIterator
            .zip(encoders)
            .map { case (value, encoder) => encoder.asInstanceOf[JdbcEncoder[Any]].encode(value) }
            .toSeq
        )

      override def unsafeSetValue(ps: PreparedStatement, index: Int, value: A): Int =
        value.productIterator
          .zip(encoders)
          .foldLeft(index) { case (i, (value, encoder)) =>
            encoder.asInstanceOf[JdbcEncoder[Any]].unsafeSetValue(ps, i, value)
          }

      override def unsafeSetNull(ps: PreparedStatement, index: Int): Int =
        encoders
          .foldLeft(index) { case (i, encoder) =>
            encoder.asInstanceOf[JdbcEncoder[Any]].unsafeSetNull(ps, i)
          }

      override def sql(a: A): String = Seq.fill(encoders.size)("?").mkString(", ")

      override def prettyValuePrinter(value: A): String = value.productIterator.mkString(", ")
    }
}

trait JdbcEncoder0LowPriorityImplicits { self =>
  private[jdbc] def primitiveCodec[A](standardType: StandardType[A]): JdbcEncoder[A] =
    standardType match {
      case StandardType.StringType     => JdbcEncoder.stringEncoder
      case StandardType.BoolType       => JdbcEncoder.booleanEncoder
      case StandardType.ShortType      => JdbcEncoder.shortEncoder
      case StandardType.IntType        => JdbcEncoder.intEncoder
      case StandardType.LongType       => JdbcEncoder.longEncoder
      case StandardType.FloatType      => JdbcEncoder.floatEncoder
      case StandardType.DoubleType     => JdbcEncoder.doubleEncoder
      case StandardType.CharType       => JdbcEncoder.charEncoder
      case StandardType.BigIntegerType => JdbcEncoder.bigIntEncoder
      case StandardType.BinaryType     => JdbcEncoder.byteChunkEncoder
      case StandardType.BigDecimalType => JdbcEncoder.bigDecimalEncoder
      case StandardType.UUIDType       => JdbcEncoder.uuidParamEncoder
      // TODO: Standard Types which are missing are the date time types, not sure what would be the best way to handle them
      case _                           => throw JdbcEncoderError(s"Unsupported type: $standardType", new IllegalArgumentException)
    }

  //scalafmt: { maxColumn = 325, optIn.configStyleArguments = false }
  def fromSchema[A](implicit schema: Schema[A]): JdbcEncoder[A] =
    schema match {
      case Schema.Primitive(standardType, _) =>
        primitiveCodec(standardType)
      case Schema.Optional(schema, _)        =>
        JdbcEncoder.optionParamEncoder(self.fromSchema(schema))
      case Schema.Tuple2(left, right, _)     =>
        JdbcEncoder.tuple2Encoder(self.fromSchema(left), self.fromSchema(right))
      // format: off
      case x@(
        _: Schema.CaseClass1[_, _] |
        _: Schema.CaseClass2[_, _, _] |
        _: Schema.CaseClass3[_, _, _, _] |
        _: Schema.CaseClass4[_, _, _, _, _] |
        _: Schema.CaseClass5[_, _, _, _, _, _] |
        _: Schema.CaseClass6[_, _, _, _, _, _, _] |
        _: Schema.CaseClass7[_, _, _, _, _, _, _, _] |
        _: Schema.CaseClass8[_, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        ) =>
        // format: on
        caseClassEncoder(x.asInstanceOf[Schema.Record[A]].fields)
      case _                                 =>
        throw JdbcEncoderError(s"Failed to encode schema ${schema}", new IllegalArgumentException)
    }

  private[jdbc] def caseClassEncoder[A](fields: Chunk[Schema.Field[A, _]]): JdbcEncoder[A] = new JdbcEncoder[A] {
    override def encode(a: A): SqlFragment =
      fields.map { f =>
        val encoder = self.fromSchema(f.schema.asInstanceOf[Schema[Any]])
        encoder.encode(f.get(a))
      }.reduce(_ ++ SqlFragment.comma ++ _)

    override def unsafeSetValue(ps: PreparedStatement, index: Int, value: A): Int =
      fields.foldLeft(index) { case (i, f) =>
        val encoder = self.fromSchema(f.schema.asInstanceOf[Schema[Any]])
        encoder.unsafeSetValue(ps, i, f.get(value))
      }

    override def unsafeSetNull(ps: PreparedStatement, index: Int): Int =
      fields.foldLeft(index) { case (i, f) =>
        val encoder = self.fromSchema(f.schema.asInstanceOf[Schema[Any]])
        encoder.unsafeSetNull(ps, i)
      }

    override def sql(a: A): String = Seq.fill(fields.iterator.size)("?").mkString(",")
  }

}
