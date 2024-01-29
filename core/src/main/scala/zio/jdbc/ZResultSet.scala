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

import zio.{ URIO, _ }

import java.sql.ResultSet

/**
 * A `ZResultSet` is a straightforward wrapper around `java.sql.ResultSet`. In order
 * to avoid needless duplication of code, one can safely access the underlying JDBC
 * `ResultSet` through the `access` method. Any such access will be attempted on the
 * blocking thread pool.
 */
final class ZResultSet(private[jdbc] val resultSet: ResultSet) {
  def access[A](f: ResultSet => A): ZIO[Any, Throwable, A] = ZIO.attemptBlocking(f(resultSet))

  def close: URIO[Any, Unit] =
    ZIO.attempt(resultSet.close()).ignoreLogged

  private[jdbc] def next(): Boolean = resultSet.next()
}
object ZResultSet                                              {
  def apply(resultSet: ResultSet): ZResultSet = new ZResultSet(resultSet)
}
