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
 * An interpolator for SQL strings, which produces `SqlFragment` values.
 *
 * @param context The `StringContext` on which the string interpolator is added.
 */

final class SqlInterpolator(val context: StringContext) extends AnyVal {

  def sql(params: SqlFragment.Segment*): SqlFragment = SqlFragment.fromFunction { chunkBuilder =>
    val syntaxIterator = context.parts.iterator
    val paramsIterator = params.iterator

    while (syntaxIterator.hasNext) {
      val syntax = syntaxIterator.next()
      if (syntax.nonEmpty) {
        chunkBuilder += SqlFragment.Segment.Syntax(syntax)
        if (paramsIterator.hasNext) chunkBuilder += paramsIterator.next()
      }
    }
    while (paramsIterator.hasNext)
      chunkBuilder += paramsIterator.next()
  }

}
