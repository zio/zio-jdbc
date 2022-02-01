package zio.jdbc

import zio.Chunk

import java.sql._

final case class SqlStatement[+A] private[jdbc] (
  private[jdbc] val segments: Chunk[SqlStatement.Segment],
  private[jdbc] val decode: ZResultSet => A
) { self =>
  import SqlStatement.Segment._

  def +(that: SqlStatement[ZResultSet])(implicit ev: A <:< ZResultSet): SqlStatement[ZResultSet] =
    new SqlStatement(self.segments ++ that.segments, that.decode)

  def as[B](implicit decode: JdbcDecoder[B]): SqlStatement[B] =
    new SqlStatement(segments, (rs: ZResultSet) => decode.decode(rs.resultSet))

  def map[B](f: A => B): SqlStatement[B] =
    new SqlStatement(segments, rs => f(decode(rs)))

  def values[B](
    iterable: Iterable[B]
  )(implicit encode: JdbcEncoder[B], ev: A <:< ZResultSet): SqlStatement[ZResultSet] =
    SqlStatement.values +
      SqlStatement.intersperse(
        SqlStatement.comma,
        iterable.map(b => SqlStatement.lparen + encode.encode(b) + SqlStatement.rparen)
      )

  def values[B](
    bs: B*
  )(implicit encode: JdbcEncoder[B], ev: A <:< ZResultSet): SqlStatement[ZResultSet] = values(bs.toIterable)

  def withDecode[B](f: ZResultSet => B): SqlStatement[B] =
    SqlStatement(segments, f)

  private[jdbc] def toStatement(conn: Connection): PreparedStatement = {
    val stringBuilder = new StringBuilder()

    var i = 0

    while (i < segments.length) {
      segments(i) match {
        case Syntax(value) => stringBuilder.append(value)
        case _             =>
      }
      i += 1
    }

    val statement = conn.prepareStatement(stringBuilder.toString)

    i = 0
    var paramIndex = 1

    while (i < segments.length) {
      segments(i) match {
        case Param(value) =>
          value match {
            case v: String                => statement.setString(paramIndex, v)
            case v: Int                   => statement.setInt(paramIndex, v)
            case v: Long                  => statement.setLong(paramIndex, v)
            case v: Short                 => statement.setShort(paramIndex, v)
            case v: Byte                  => statement.setByte(paramIndex, v)
            case v: Char                  => statement.setString(paramIndex, v.toString)
            case v: Double                => statement.setDouble(paramIndex, v)
            case v: Blob                  => statement.setBlob(paramIndex, v)
            case v: java.math.BigDecimal  => statement.setBigDecimal(paramIndex, v)
            case v: scala.math.BigDecimal => statement.setBigDecimal(paramIndex, v.bigDecimal)
            case v                        => statement.setString(paramIndex, v.toString())
          }

          paramIndex += 1

        case Syntax(_) =>
      }
      i += 1
    }

    statement
  }
}
object SqlStatement {
  val empty: SqlStatement[ZResultSet] = SqlStatement(Chunk.empty, identity(_))

  sealed trait Segment
  object Segment {
    final case class Syntax(value: String) extends Segment
    final case class Param(value: Any)     extends Segment
  }

  private[jdbc] def intersperse(
    sep: SqlStatement[ZResultSet],
    elements: Iterable[SqlStatement[ZResultSet]]
  ): SqlStatement[ZResultSet] = {

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
