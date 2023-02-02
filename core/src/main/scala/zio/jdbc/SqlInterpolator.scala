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

/**
 * An interpolator for SQL strings, which produces `Sql` values.
 *
 * @param context The `StringContext` on which the string interpolator is added.
 */

final class SqlInterpolator(val context: StringContext) extends AnyVal {
  def sql(params: Any*): SqlFragment = new Sql(
    chunkBuilder => {
      import Sql.Segment

      val partsIterator  = context.parts.toIterable.iterator
      val paramsIterator = params.toIterable.iterator

      while (partsIterator.hasNext) {
        val next = partsIterator.next()
        if (next.nonEmpty) {
          chunkBuilder += Segment.Syntax(next)
          if (paramsIterator.hasNext) paramsIterator.next() match {
            case sql: Sql[Any] => sql.build(chunkBuilder)
            case param         => chunkBuilder += Segment.Param(param)
          }
        }
      }
      while (paramsIterator.hasNext)
        chunkBuilder += Segment.Param(paramsIterator.next())
    },
    Sql.identityFn
  )
}
