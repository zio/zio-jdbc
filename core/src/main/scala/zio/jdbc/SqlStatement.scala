package zio.jdbc

import zio.Chunk

import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.Blob

final class SqlStatement[+A] private[jdbc] (
  parts: Chunk[String],
  args: Chunk[Any],
  private[jdbc] val decode: ZResultSet => A
) { self =>
  def as[B](implicit decode: JdbcDecoder[B]): SqlStatement[B] =
    new SqlStatement(parts, args, (rs: ZResultSet) => decode.decode(rs.resultSet))

  def map[B](f: A => B): SqlStatement[B] =
    new SqlStatement(parts, args, rs => f(decode(rs)))

  private[jdbc] def toStatement(conn: Connection): PreparedStatement = {
    val stringBuilder = new StringBuilder()

    val partsIterator = parts.iterator
    val argsArray     = args

    var argsIndex = 0

    while (partsIterator.hasNext) {
      val part = partsIterator.next()

      stringBuilder.append(part)

      if (argsIndex < argsArray.length) {
        argsIndex += 1

        stringBuilder.append("?")
      }
    }

    while (argsIndex < argsArray.length) {
      argsIndex += 1

      stringBuilder.append("?")
    }

    val sql = stringBuilder.toString

    val statement = conn.prepareStatement(sql)

    argsIndex = 0
    while (argsIndex < argsArray.length) {
      argsArray(argsIndex) match {
        case v: String                => statement.setString(argsIndex + 1, v)
        case v: Int                   => statement.setInt(argsIndex + 1, v)
        case v: Long                  => statement.setLong(argsIndex + 1, v)
        case v: Short                 => statement.setShort(argsIndex + 1, v)
        case v: Byte                  => statement.setByte(argsIndex + 1, v)
        case v: Char                  => statement.setString(argsIndex + 1, v.toString)
        case v: Double                => statement.setDouble(argsIndex + 1, v)
        case v: Blob                  => statement.setBlob(argsIndex + 1, v)
        case v: java.math.BigDecimal  => statement.setBigDecimal(argsIndex + 1, v)
        case v: scala.math.BigDecimal => statement.setBigDecimal(argsIndex + 1, v.bigDecimal)
        case v                        => statement.setString(argsIndex + 1, v.toString())
      }

      argsIndex += 1
    }

    statement
  }
}
