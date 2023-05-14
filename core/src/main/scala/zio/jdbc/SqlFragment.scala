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

  def and(): SqlFragment = self ++ SqlFragment.and

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
      sql.append(param.setter.sql(param.value))
      paramsBuilder += param.setter.prettyValuePrinter(param.value)
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
  def execute: ZIO[ZConnection, Throwable, Unit] =
    ZIO.scoped(for {
      connection <- ZIO.service[ZConnection]
      _          <- connection.executeSqlWith(self) { ps =>
                      ZIO.attempt(ps.executeUpdate())
                    }
    } yield ())

  /**
   * Executes a SQL delete query.
   */
  def delete: ZIO[ZConnection, Throwable, Long] =
    ZIO.scoped(executeLargeUpdate(self))

  /**
   * Performs an SQL insert query, returning a count of rows inserted and a
   * [[zio.Chunk]] of auto-generated keys. By default, auto-generated keys are
   * parsed and returned as `Chunk[Long]`. If keys are non-numeric, a
   * `Chunk.empty` is returned.
   */
  def insert: ZIO[ZConnection, Throwable, UpdateResult] =
    ZIO.scoped(executeWithUpdateResult(self))

  /**
   * Performs a SQL update query, returning a count of rows updated.
   */
  def update: ZIO[ZConnection, Throwable, Long] =
    ZIO.scoped(executeLargeUpdate(self))

  private def executeLargeUpdate(sql: SqlFragment): ZIO[Scope with ZConnection, Throwable, Long] = for {
    connection <- ZIO.service[ZConnection]
    count      <- connection.executeSqlWith(sql) { ps =>
                    ZIO.attempt(ps.executeLargeUpdate())
                  }
  } yield count

  private def executeWithUpdateResult(sql: SqlFragment): ZIO[Scope with ZConnection, Throwable, UpdateResult] =
    for {
      connection <- ZIO.service[ZConnection]
      result     <- connection.executeSqlWith(sql) { ps =>
                      for {
                        result     <- ZIO.acquireRelease(ZIO.attempt {
                                        val rowsUpdated = ps.executeLargeUpdate()
                                        val updatedKeys = ps.getGeneratedKeys
                                        (rowsUpdated, ZResultSet(updatedKeys))
                                      })(_._2.close)
                        (count, rs) = result
                        keys       <- ZIO.attempt {
                                        val builder = ChunkBuilder.make[Long]()
                                        while (rs.next())
                                          builder += rs.resultSet.getLong(1)
                                        builder.result()
                                      }.orElseSucceed(Chunk.empty)

                      } yield UpdateResult(count, keys)
                    }
    } yield result

  private[jdbc] def foreachSegment(addSyntax: Segment.Syntax => Any)(addParam: Segment.Param => Any): Unit =
    segments.foreach {
      case syntax: Segment.Syntax => addSyntax(syntax)
      case param: Segment.Param   => addParam(param)
      case nested: Segment.Nested => nested.sql.foreachSegment(addSyntax)(addParam)
    }

}

object SqlFragment {

  val empty: SqlFragment = SqlFragment(Chunk.empty[Segment])

  def fromFunction(f: ChunkBuilder[Segment] => Unit): SqlFragment =
    SqlFragment.FromFunction(f)

  sealed trait Segment
  object Segment {
    final case class Syntax(value: String)                       extends Segment
    final case class Param(value: Any, setter: JdbcEncoder[Any]) extends Segment
    final case class Nested(sql: SqlFragment)                    extends Segment

    implicit def paramSegment[A](a: A)(implicit encoder: JdbcEncoder[A]): Segment.Param =
      Segment.Param(a, encoder.asInstanceOf[JdbcEncoder[Any]])

//    implicit def paramSegment[A](a: A)(implicit setter: JdbcEncoder[A]): Segment =
//      Nested(setter.encode(a))

    implicit def nestedSqlSegment[A](sql: SqlFragment): Segment.Nested = Segment.Nested(sql)
  }

  def apply(sql: String): SqlFragment = sql

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

  def intersperse(
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
