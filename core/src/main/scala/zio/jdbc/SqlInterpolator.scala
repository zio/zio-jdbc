package zio.jdbc

final class SqlInterpolator(val context: StringContext) extends AnyVal {
  def sql(args: Any*): SqlStatement[ZResultSet] = new SqlStatement(context.parts.toArray, args.toArray, identity(_))
}
