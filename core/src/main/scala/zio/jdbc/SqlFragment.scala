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
import zio.jdbc.SqlFragment.Segment

import java.sql.{ PreparedStatement, SQLException, SQLTimeoutException, Types }
import java.time.{ OffsetDateTime, ZoneOffset }
import scala.language.implicitConversions

/**
 * A `SqlFragment` represents part or all of a SQL query. The SQL
 * is described by a sequence of segments, each segment being either a
 * fragment of SQL, or a value to be inserted into the query in a way that
 * is safe from SQL injection attacks.
 *
 * @param segments
 * @param decode
 */
sealed trait SqlFragment { self =>

  def ++(that: SqlFragment): SqlFragment =
    SqlFragment.AndThen(self, that)

  def and(first: SqlFragment, rest: SqlFragment*): SqlFragment =
    and(first +: rest)

  def and(elements: Iterable[SqlFragment]): SqlFragment =
    self ++ SqlFragment.prependEach(SqlFragment.and, elements)

  final def build(builder: ChunkBuilder[Segment]): Unit = {

    val stack = zio.internal.Stack[SqlFragment]()

    var currentSqlFragment = self

    while (currentSqlFragment ne null)
      currentSqlFragment match {
        case SqlFragment.AndThen(left, right) =>
          stack.push(right)
          currentSqlFragment = left
        case SqlFragment.Append(segments)     =>
          builder ++= segments
          currentSqlFragment = stack.pop()
        case SqlFragment.FromFunction(f)      =>
          f(builder)
          currentSqlFragment = stack.pop()
      }
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: SqlFragment => self.segments == that.segments
      case _                 => false
    }

  def from(table: SqlFragment): SqlFragment =
    self ++ SqlFragment.from ++ table

  override def hashCode: Int = segments.hashCode

  def in[B](b: B, bs: B*)(implicit encoder: JdbcEncoder[B]): SqlFragment =
    in(b +: bs)

  def in[B](bs: Iterable[B])(implicit encoder: JdbcEncoder[B]): SqlFragment =
    in0(SqlFragment.in, bs)

  def not(fragment: SqlFragment): SqlFragment =
    self ++ SqlFragment.not ++ fragment

  def notIn[B](b: B, bs: B*)(implicit encoder: JdbcEncoder[B]): SqlFragment =
    notIn(b +: bs)

  def notIn[B](bs: Iterable[B])(implicit encoder: JdbcEncoder[B]): SqlFragment =
    in0(SqlFragment.notIn, bs)

  private def in0[B](op: SqlFragment, bs: Iterable[B])(implicit encoder: JdbcEncoder[B]): SqlFragment =
    self ++ op ++ SqlFragment.lparen ++ SqlFragment.intersperse(
      SqlFragment.comma,
      bs.map(encoder.encode)
    ) ++ SqlFragment.rparen

  def or(first: SqlFragment, rest: SqlFragment*): SqlFragment =
    or(first +: rest)

  def or(elements: Iterable[SqlFragment]): SqlFragment =
    self ++ SqlFragment.prependEach(SqlFragment.or, elements)

  def segments: Chunk[Segment] = {
    val builder = ChunkBuilder.make[Segment]()
    build(builder)
    builder.result()
  }

  override def toString: String = {
    val sql           = new StringBuilder()
    val paramsBuilder = ChunkBuilder.make[String]()

    foreachSegment { syntax =>
      sql.append(syntax.value)
    } { param =>
      var size = 0
      param.value match {
        case iterable: Iterable[_] =>
          iterable.foreach { item =>
            paramsBuilder += item.toString
            size += 1
          }

        case array: Array[_] =>
          array.foreach { item =>
            paramsBuilder += item.toString
            size += 1
          }

        case _ =>
          paramsBuilder += param.value.toString
          size += 1
      }
      sql.append(
        if (size == 1) "?"
        else Seq.fill(size)("?").mkString(",")
      )
    }

    val params       = paramsBuilder.result()
    val paramsString = if (params.isEmpty) "" else ", " + params.mkString(", ")

    s"Sql(${sql.result()}$paramsString)"
  }

  def values[B](bs: Iterable[B])(implicit encoder: JdbcEncoder[B]): SqlFragment =
    this ++
      SqlFragment.values ++
      SqlFragment.intersperse(
        SqlFragment.comma,
        bs.map(b => SqlFragment.lparen ++ encoder.encode(b) ++ SqlFragment.rparen)
      )

  def valuesBatched[B](bs: Iterable[B], batchSize: Int = 2000, tableName: String)(
    keys: String*
  )(implicit encoder: JdbcEncoder[B]): Seq[SqlFragment] = {
    val batches          = bs.grouped(batchSize)
    val insertStatements =
      batches.map(batch => SqlFragment.insertInto(tableName)(keys.mkString(", ")).values(batch)).toSeq
    insertStatements
  }

  def values[B](b: B, bs: B*)(implicit encoder: JdbcEncoder[B]): SqlFragment =
    values(b +: bs)

  def where(predicate: SqlFragment): SqlFragment =
    self ++ SqlFragment.where ++ predicate

  def query[A: JdbcDecoder]: Query[A] =
    Query.fromSqlFragment(self)

  /**
   * Executes a SQL statement, such as one that creates a table.
   */
  def execute: ZIO[ZConnection, QueryException, Unit] =
    ZIO
      .scoped(for {
        connection <- ZIO.service[ZConnection]
        _          <- connection.executeSqlWith(self, false) { ps =>
                        ZIO.attempt(ps.executeUpdate()).refineOrDie {
                          case e: SQLTimeoutException => ZSQLTimeoutException(e)
                          case e: SQLException        => ZSQLException(e)
                        }
                      }
      } yield ())

  /**
   * Executes a SQL delete query.
   */
  def delete: ZIO[ZConnection, QueryException, Long] =
    ZIO.scoped(executeLargeUpdate(self))

  /**
   * Executes a SQL delete query with a RETURNING clause, materialized
   * as values of type `A`.
   */
  def deleteReturning[A: JdbcDecoder]: ZIO[ZConnection, Throwable, UpdateResult[A]] =
    ZIO.scoped(executeWithReturning(self, JdbcDecoder[A]))

  /**
   * Performs an SQL insert query, returning a count of rows inserted.
   */
  def insert: ZIO[ZConnection, Throwable, Long] =
    ZIO.scoped(executeUpdate(self, false).map(_._1))

  /**
   * Executes a SQL insert query with a RETURNING clause, materialized
   * as values of type `A`.
   */
  def insertReturning[A: JdbcDecoder]: ZIO[ZConnection, Throwable, UpdateResult[A]] =
    ZIO.scoped(executeWithReturning(self, JdbcDecoder[A]))

  /**
   * Performs an SQL insert query, returning a count of rows inserted and a
   * [[zio.Chunk]] of auto-generated keys. By default, auto-generated keys are
   * parsed and returned as `Chunk[Long]`. If keys are non-numeric, a
   * `Chunk.empty` is returned.
   */
  def insertWithKeys: ZIO[ZConnection, QueryException, UpdateResult[Long]] =
    ZIO.scoped(executeWithReturning(self, JdbcDecoder[Long]))

  /**
   * Executes a SQL update query with a RETURNING clause, materialized
   * as values of type `A`.
   */
  def updateReturning[A: JdbcDecoder]: ZIO[ZConnection, QueryException, UpdateResult[A]] =
    ZIO.scoped(executeWithReturning(self, JdbcDecoder[A]))

  /**
   * Performs a SQL update query, returning a count of rows updated.
   */
  def update: ZIO[ZConnection, QueryException, Long] =
    ZIO.scoped(executeLargeUpdate(self))

  private def executeLargeUpdate(sql: SqlFragment): ZIO[Scope with ZConnection, QueryException, Long] = for {
    connection <- ZIO.service[ZConnection]
    count      <- connection.executeSqlWith(sql, false) { ps =>
                    ZIO.attempt(ps.executeLargeUpdate()).refineOrDie {
                      case e: SQLTimeoutException => ZSQLTimeoutException(e)
                      case e: SQLException        => ZSQLException(e)
                    }
                  }
  } yield count

  private def executeWithReturning[A](
    sql: SqlFragment,
    decoder: JdbcDecoder[A]
  ): ZIO[Scope with ZConnection, QueryException, UpdateResult[A]] =
    for {
      updateRes       <- executeUpdate(sql, true)
      (count, maybeRs) = updateRes
      keys            <- maybeRs match {
                           case None     => ZIO.succeed(Chunk.empty)
                           case Some(rs) =>
                             ZIO.attempt {
                               val builder = ChunkBuilder.make[A]()
                               while (rs.next())
                                 builder += decoder.unsafeDecode(1, rs.resultSet)._2
                               builder.result()
                             }.refineOrDie {
                               case e: SQLTimeoutException => ZSQLTimeoutException(e)
                               case e: SQLException        => ZSQLException(e)
                             }
                         }
    } yield UpdateResult(count, keys)

  private[jdbc] def executeUpdate(
    sql: SqlFragment,
    returnAutoGeneratedKeys: Boolean
  ): ZIO[Scope with ZConnection, QueryException, (Long, Option[ZResultSet])] =
    for {
      connection <- ZIO.service[ZConnection]
      result     <- connection.executeSqlWith(sql, returnAutoGeneratedKeys) { ps =>
                      ZIO
                        .acquireRelease(ZIO.attempt {
                          val rowsUpdated = ps.executeLargeUpdate()
                          val updatedKeys = if (returnAutoGeneratedKeys) Some(ps.getGeneratedKeys) else None
                          (rowsUpdated, updatedKeys.map(ZResultSet(_)))

                        })(_._2.map(_.close).getOrElse(ZIO.unit))
                        .refineOrDie {
                          case e: SQLTimeoutException => ZSQLTimeoutException(e)
                          case e: SQLException        => ZSQLException(e)
                        }
                    }
    } yield result

  private[jdbc] def foreachSegment(addSyntax: Segment.Syntax => Any)(addParam: Segment.Param => Any): Unit =
    segments.foreach {
      case Segment.Empty          => ()
      case syntax: Segment.Syntax => addSyntax(syntax)
      case param: Segment.Param   => addParam(param)
      case nested: Segment.Nested => nested.sql.foreachSegment(addSyntax)(addParam)
    }

}

object SqlFragment {

  val empty: SqlFragment = SqlFragment(Chunk.empty[Segment])

  def fromFunction(f: ChunkBuilder[Segment] => Unit): SqlFragment =
    SqlFragment.FromFunction(f)

  implicit final class FragmentOps[I[t] <: Iterable[t]](private val fragments: I[SqlFragment]) extends AnyVal {
    def mkFragment(sep: SqlFragment): SqlFragment =
      intersperse(sep, fragments)

    def mkFragment(start: SqlFragment, sep: SqlFragment, end: SqlFragment): SqlFragment =
      start ++ fragments.mkFragment(sep) ++ end
  }

  implicit final class NonEmptyChunkOps(private val fragments: NonEmptyChunk[SqlFragment]) extends AnyVal {
    def mkFragment(sep: SqlFragment): SqlFragment =
      fragments.toChunk.mkFragment(sep)

    def mkFragment(start: SqlFragment, sep: SqlFragment, end: SqlFragment): SqlFragment =
      fragments.toChunk.mkFragment(start, sep, end)
  }

  sealed trait Segment
  object Segment {
    case object Empty                                       extends Segment
    final case class Syntax(value: String)                  extends Segment
    final case class Param(value: Any, setter: Setter[Any]) extends Segment
    final case class Nested(sql: SqlFragment)               extends Segment

    @inline def empty: Segment = Empty

    implicit def paramSegment[A](a: A)(implicit setter: Setter[A]): Segment.Param =
      Segment.Param(a, setter.asInstanceOf[Setter[Any]])

    implicit def nestedSqlSegment[A](sql: SqlFragment): Segment.Nested = Segment.Nested(sql)
  }

  trait Setter[A] { self =>
    def setValue(ps: PreparedStatement, index: Int, value: A): Unit
    def setNull(ps: PreparedStatement, index: Int): Unit

    final def contramap[B](f: B => A): Setter[B] =
      Setter((ps, i, value) => self.setValue(ps, i, f(value)), (ps, i) => self.setNull(ps, i))
  }

  object Setter {
    def apply[A](implicit setter: Setter[A]): Setter[A] = setter

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

    implicit def chunkSetter[A](implicit setter: Setter[A]): Setter[Chunk[A]]   = iterableSetter[A, Chunk[A]]
    implicit def listSetter[A](implicit setter: Setter[A]): Setter[List[A]]     = iterableSetter[A, List[A]]
    implicit def vectorSetter[A](implicit setter: Setter[A]): Setter[Vector[A]] = iterableSetter[A, Vector[A]]
    implicit def setSetter[A](implicit setter: Setter[A]): Setter[Set[A]]       = iterableSetter[A, Set[A]]

    private def iterableSetter[A, I <: Iterable[A]](implicit setter: Setter[A]): Setter[I] =
      forSqlType(
        (ps, i, iterable) =>
          iterable.zipWithIndex.foreach { case (value, valueIdx) =>
            setter.setValue(ps, i + valueIdx, value)
          },
        Types.OTHER
      )

    implicit val bigDecimalSetter: Setter[java.math.BigDecimal] =
      forSqlType((ps, i, value) => ps.setBigDecimal(i, value), Types.NUMERIC)

    implicit val uuidParamSetter: Setter[java.util.UUID] = other((ps, i, value) => ps.setObject(i, value), "uuid")

    implicit val charSetter: Setter[Char]                             = stringSetter.contramap(_.toString)
    implicit val bigIntSetter: Setter[java.math.BigInteger]           = bigDecimalSetter.contramap(new java.math.BigDecimal(_))
    implicit val bigDecimalScalaSetter: Setter[scala.math.BigDecimal] = bigDecimalSetter.contramap(_.bigDecimal)
    implicit val byteChunkSetter: Setter[Chunk[Byte]]                 = byteArraySetter.contramap(_.toArray)

    // These `java.time.*` are inspired from Quill encoders. See `ObjectGenericTimeEncoders` in Quill.
    // Notes:
    //   1. These setters probably don't work for SQLite. Quill as a separate trait, named `BasicTimeDecoders` which seems dedicated to SQLite.
    //   2. We deliberately decided not to support `java.time.OffsetTime`.
    //      Because:
    //        - See https://github.com/h2database/h2database/issues/521#issuecomment-333517705
    //        - It's supposed to be mapped to `java.sql.Types.TIME_WITH_TIMEZONE` but this type isn't supported by the PG JDBC driver.
    //          See: https://github.com/pgjdbc/pgjdbc/blob/9cf9f36a1d3a1edd9286721f9c0b9cfa9e8422e3/pgjdbc/src/main/java/org/postgresql/jdbc/PgPreparedStatement.java#L557-L741
    //      Note that Quill made a different choice. For PG, it uses `java.sql.Types.TIME` but as we don't support yet differences between DBs and `OffsetTime` is almost never used
    //      it's simpler, for now, to not support it and to document this choice.
    //      If you need it, please open an issue or a PR explaining your use case.
    implicit val sqlDateSetter: Setter[java.sql.Date]                   = forSqlType((ps, i, value) => ps.setDate(i, value), Types.DATE)
    implicit val sqlTimeSetter: Setter[java.sql.Time]                   = forSqlType((ps, i, value) => ps.setTime(i, value), Types.TIME)
    implicit val sqlTimestampSetter: Setter[java.sql.Timestamp]         =
      forSqlType((ps, i, value) => ps.setTimestamp(i, value), Types.TIMESTAMP)
    implicit val localDateSetter: Setter[java.time.LocalDate]           =
      sqlDateSetter.contramap(java.sql.Date.valueOf)
    implicit val localTimeSetter: Setter[java.time.LocalTime]           =
      sqlTimeSetter.contramap(java.sql.Time.valueOf)
    implicit val localDateTimeSetter: Setter[java.time.LocalDateTime]   =
      sqlTimestampSetter.contramap(java.sql.Timestamp.valueOf)
    implicit val zonedDateTimeSetter: Setter[java.time.ZonedDateTime]   =
      forSqlType(
        (ps, i, value) => ps.setObject(i, value.toOffsetDateTime, Types.TIMESTAMP_WITH_TIMEZONE),
        Types.TIMESTAMP_WITH_TIMEZONE
      )
    implicit val instantSetter: Setter[java.time.Instant]               =
      forSqlType(
        (ps, i, value) => ps.setObject(i, OffsetDateTime.ofInstant(value, ZoneOffset.UTC)),
        Types.TIMESTAMP_WITH_TIMEZONE
      )
    implicit val offsetDateTimeSetter: Setter[java.time.OffsetDateTime] =
      forSqlType((ps, i, value) => ps.setObject(i, value, Types.TIMESTAMP_WITH_TIMEZONE), Types.TIMESTAMP_WITH_TIMEZONE)
  }

  def apply(sql: String): SqlFragment = SqlFragment(Chunk.single(SqlFragment.Segment.Syntax(sql)))

  def apply(segments: Chunk[Segment]): SqlFragment =
    SqlFragment.Append(segments)

  def deleteFrom(table: String): SqlFragment =
    s"DELETE FROM $table"

  def insertInto(table: String)(keys: String*): SqlFragment =
    s"INSERT INTO $table (${keys.mkString(", ")})"

  def select(columns: String*): SqlFragment =
    s"SELECT ${columns.mkString(", ")}"

  def update(table: String): SqlFragment =
    s"UPDATE $table"

  def intersperse(sep: SqlFragment, elements: Iterable[SqlFragment]): SqlFragment = {
    var first = true
    elements.foldLeft(empty) { (acc, element) =>
      if (!first) acc ++ sep ++ element
      else {
        first = false
        acc ++ element
      }
    }
  }

  private[jdbc] def prependEach(sep: SqlFragment, elements: Iterable[SqlFragment]): SqlFragment =
    elements.foldLeft(empty) { (acc, element) =>
      acc ++ sep ++ element
    }

  private[jdbc] val and         = sql" AND "
  private[jdbc] val comma       = sql","
  private[jdbc] val from        = sql" FROM "
  private[jdbc] val in          = sql" IN "
  private[jdbc] val lparen      = sql"("
  private[jdbc] val not         = sql" NOT "
  private[jdbc] val notIn       = sql" NOT IN "
  private[jdbc] val nullLiteral = sql"NULL"
  private[jdbc] val or          = sql" OR "
  private[jdbc] val rparen      = sql")"
  private[jdbc] val values      = sql" VALUES "
  private[jdbc] val where       = sql" WHERE "

  private final case class AndThen(left: SqlFragment, right: SqlFragment) extends SqlFragment
  private final case class Append(override val segments: Chunk[Segment])  extends SqlFragment
  private final case class FromFunction(f: ChunkBuilder[Segment] => Unit) extends SqlFragment
}
