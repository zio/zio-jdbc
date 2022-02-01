package zio.jdbc

import zio.Chunk

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
final case class Sql[+A](
  val segments: Chunk[Sql.Segment],
  val decode: ZResultSet => A
) { self =>
  def +(that: Sql[ZResultSet])(implicit ev: A <:< ZResultSet): Sql[ZResultSet] =
    new Sql(self.segments ++ that.segments, that.decode)

  def as[B](implicit decode: JdbcDecoder[B]): Sql[B] =
    new Sql(segments, (rs: ZResultSet) => decode.unsafeDecode(rs.resultSet))

  def map[B](f: A => B): Sql[B] =
    new Sql(segments, rs => f(decode(rs)))

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
}
