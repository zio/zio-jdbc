/*
 * Copyright 2023 John A. De Goes and the ZIO Contributors
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

import zio._

import java.sql.Connection

sealed trait TransactionIsolationLevel extends ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] { self =>

  def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
    currentTransactionIsolationLevel.locally(self)(zio)

  def toInt: Int =
    self match {
      case TransactionIsolationLevel.None            => Connection.TRANSACTION_NONE
      case TransactionIsolationLevel.ReadUncommitted => Connection.TRANSACTION_READ_UNCOMMITTED
      case TransactionIsolationLevel.ReadCommitted   => Connection.TRANSACTION_READ_COMMITTED
      case TransactionIsolationLevel.RepeatableRead  => Connection.TRANSACTION_REPEATABLE_READ
      case TransactionIsolationLevel.Serializable    => Connection.TRANSACTION_SERIALIZABLE
    }
}

object TransactionIsolationLevel {
  case object None            extends TransactionIsolationLevel
  case object ReadUncommitted extends TransactionIsolationLevel
  case object ReadCommitted   extends TransactionIsolationLevel
  case object RepeatableRead  extends TransactionIsolationLevel
  case object Serializable    extends TransactionIsolationLevel
}
