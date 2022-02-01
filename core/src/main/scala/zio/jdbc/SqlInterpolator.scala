package zio.jdbc

import zio.ChunkBuilder

final class SqlInterpolator(val context: StringContext) extends AnyVal {
  def sql(params: Any*): SqlStatement[ZResultSet] = {
    import SqlStatement.Segment

    val chunkBuilder = ChunkBuilder.make[Segment]()

    val partsIterator  = context.parts.toIterable.iterator
    val paramsIterator = params.toIterable.iterator

    while (partsIterator.hasNext) {
      chunkBuilder += Segment.Syntax(partsIterator.next())

      if (paramsIterator.hasNext) {
        chunkBuilder += Segment.Param(paramsIterator.next())
      }
    }

    while (paramsIterator.hasNext)
      chunkBuilder += Segment.Param(paramsIterator.next())

    new SqlStatement(chunkBuilder.result(), identity(_))
  }
}
