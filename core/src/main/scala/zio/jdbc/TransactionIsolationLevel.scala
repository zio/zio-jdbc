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

sealed abstract class TransactionIsolationLevel(val value: Int)

object TransactionIsolationLevel {

  case object ReadUncommitted extends TransactionIsolationLevel(1)
  case object ReadCommitted   extends TransactionIsolationLevel(2)
  case object RepeatableRead  extends TransactionIsolationLevel(4)
  case object Serializable    extends TransactionIsolationLevel(8)

  def fromInt(level: Int): Either[Throwable, TransactionIsolationLevel] =
    level match {
      case 1 => Right(ReadUncommitted)
      case 2 => Right(ReadCommitted)
      case 4 => Right(RepeatableRead)
      case 8 => Right(Serializable)
      case _ => Left(new IllegalArgumentException(s"Incorrect transaction isolation level: $level"))
    }

}
