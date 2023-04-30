package zio.jdbc

import zio._
import zio.jdbc.SqlFragment0.Segment

import java.sql.{ PreparedStatement, Types }
import scala.language.implicitConversions

final class SqlFragment0(private[jdbc] val build: ChunkBuilder[Segment] => Unit) { self =>

  def ++(that: SqlFragment0): SqlFragment0 =
    new SqlFragment0(builder => { self.build(builder); that.build(builder) })

  def and(first: SqlFragment0, rest: SqlFragment0*): SqlFragment0 =
    and(first +: rest)

  def and(elements: Iterable[SqlFragment0]): SqlFragment0 =
    self ++ SqlFragment0.prependEach(SqlFragment0.and, elements)

  override def equals(that: Any): Boolean =
    that match {
      case that: SqlFragment0 => self.segments == that.segments
      case _                  => false
    }

  def from(table: SqlFragment0): SqlFragment0 =
    self ++ SqlFragment0.from ++ table

  override def hashCode: Int = segments.hashCode

  def in[B](b: B, bs: B*)(implicit encoder: JdbcEncoder0[B]): SqlFragment0 =
    in(b +: bs)

  def in[B](bs: Iterable[B])(implicit encoder: JdbcEncoder0[B]): SqlFragment0 =
    in0(SqlFragment0.in, bs)

  def not(fragment: SqlFragment0): SqlFragment0 =
    self ++ SqlFragment0.not ++ fragment

  def notIn[B](b: B, bs: B*)(implicit encoder: JdbcEncoder0[B]): SqlFragment0 =
    notIn(b +: bs)

  def notIn[B](bs: Iterable[B])(implicit encoder: JdbcEncoder0[B]): SqlFragment0 =
    in0(SqlFragment0.notIn, bs)

  private def in0[B](op: SqlFragment0, bs: Iterable[B])(implicit encoder: JdbcEncoder0[B]): SqlFragment0 =
    self ++ op ++ SqlFragment0.lparen ++ SqlFragment0.intersperse(
      SqlFragment0.comma,
      bs.map(encoder.encode)
    ) ++ SqlFragment0.rparen

  def or(first: SqlFragment0, rest: SqlFragment0*): SqlFragment0 =
    or(first +: rest)

  def or(elements: Iterable[SqlFragment0]): SqlFragment0 =
    self ++ SqlFragment0.prependEach(SqlFragment0.or, elements)

  def segments: Chunk[Segment] = {
    val builder = ChunkBuilder.make[Segment]()
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

  def values[B](bs: Iterable[B])(implicit encoder: JdbcEncoder0[B]): SqlFragment0 =
    this ++
      SqlFragment0.values ++
      SqlFragment0.intersperse(
        SqlFragment0.comma,
        bs.map(b => SqlFragment0.lparen ++ encoder.encode(b) ++ SqlFragment0.rparen)
      )

  def valuesBatched[B](bs: Iterable[B], batchSize: Int = 2000, tableName: String)(
    keys: String*
  )(implicit encoder: JdbcEncoder0[B]): Seq[SqlFragment0] = {
    val batches          = bs.grouped(batchSize)
    val insertStatements =
      batches.map(batch => SqlFragment0.insertInto(tableName)(keys.mkString(", ")).values(batch)).toSeq
    insertStatements
  }

  def values[B](b: B, bs: B*)(implicit encoder: JdbcEncoder0[B]): SqlFragment0 =
    values(b +: bs)

  def where(predicate: SqlFragment0): SqlFragment0 =
    self ++ SqlFragment0.where ++ predicate

  def query[A](implicit decoder: JdbcDecoder[A]): Query[A] =
    new Query[A](self, zrs => decoder.unsafeDecode(zrs.resultSet))

}

object SqlFragment0 {

  val empty: SqlFragment0 = SqlFragment0(Chunk.empty[Segment])

  sealed trait Segment
  object Segment {
    final case class Syntax(value: String)                  extends Segment
    final case class Param(value: Any, setter: Setter[Any]) extends Segment
    final case class Nested(sql: SqlFragment0)              extends Segment

    implicit def paramSegment[A](a: A)(implicit setter: Setter[A]): Segment.Param =
      Segment.Param(a, setter.asInstanceOf[Setter[Any]])

    implicit def nestedSqlSegment[A](sql: SqlFragment0): Segment.Nested = Segment.Nested(sql)
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

  def apply(sql: String): SqlFragment0 = sql

  def apply(segments: Chunk[Segment]): SqlFragment0 =
    new SqlFragment0(builder => builder ++= segments)

  def deleteFrom(table: String): SqlFragment0 =
    s"DELETE FROM $table"

  def insertInto(table: String)(keys: String*): SqlFragment0 =
    s"INSERT INTO $table (${keys.mkString(", ")})"

  def select(columns: String*): SqlFragment0 =
    s"SELECT ${columns.mkString(", ")}"

  def update(table: String): SqlFragment0 =
    s"UPDATE $table"

  private[jdbc] def intersperse(
    sep: SqlFragment0,
    elements: Iterable[SqlFragment0]
  ): SqlFragment0 = {
    var first = true
    elements.foldLeft(empty) { (acc, element) =>
      if (!first) acc ++ sep ++ element
      else {
        first = false
        acc ++ element
      }
    }
  }

  private[jdbc] def prependEach(sep: SqlFragment0, elements: Iterable[SqlFragment0]): SqlFragment0 =
    elements.foldLeft(empty) { (acc, element) =>
      acc ++ sep ++ element
    }

  private[jdbc] val and         = sql0" AND "
  private[jdbc] val comma       = sql0","
  private[jdbc] val from        = sql0" FROM "
  private[jdbc] val in          = sql0" IN "
  private[jdbc] val lparen      = sql0"("
  private[jdbc] val not         = sql0" NOT "
  private[jdbc] val notIn       = sql0" NOT IN "
  private[jdbc] val nullLiteral = sql0"NULL"
  private[jdbc] val or          = sql0" OR "
  private[jdbc] val rparen      = sql0")"
  private[jdbc] val values      = sql0" VALUES "
  private[jdbc] val where       = sql0" WHERE "

}
