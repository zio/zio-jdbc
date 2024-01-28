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
package zio

import scala.language.implicitConversions

package object jdbc {

  implicit def sqlInterpolator(sc: StringContext): SqlInterpolator = new SqlInterpolator(sc)

  /**
   * Converts a String into a pure SQL expression
   */
  implicit def stringToSql(s: String): SqlFragment = SqlFragment(s)

  /**
   * A new transaction, which may be applied to ZIO effects that require a
   * connection in order to execute such effects in the transaction.
   */
  val transaction: ZLayer[ZConnectionPool, ConnectionException, ZConnection] =
    ZLayer(ZIO.serviceWith[ZConnectionPool](_.transaction)).flatten

  private[jdbc] val currentTransactionIsolationLevel: FiberRef[Option[TransactionIsolationLevel]] =
    FiberRef.unsafe.make[Option[TransactionIsolationLevel]](None)(Unsafe.unsafe)
}
