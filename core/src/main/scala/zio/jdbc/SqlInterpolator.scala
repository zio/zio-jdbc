package zio.jdbc

import zio.Chunk

final class SqlInterpolator(val context: StringContext) extends AnyVal {
  def sql(args: Any*): SqlStatement[ZResultSet] =
    new SqlStatement(Chunk.fromIterable(context.parts.toIterable), Chunk.fromIterable(args), identity(_))
}
