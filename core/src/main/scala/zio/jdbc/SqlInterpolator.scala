package zio.jdbc

import zio.ChunkBuilder

/**
 * An interpolator for SQL strings, which produces `Sql` values.
 *
 * @param context The `StringContext` on which the string interpolator is added.
 */
final class SqlInterpolator(val context: StringContext) extends AnyVal {
  def sql(params: Any*): Sql[ZResultSet] = {
    import Sql.Segment

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

    new Sql(chunkBuilder.result(), Sql.identityFn)
  }
}
