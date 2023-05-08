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

import zio._
import zio.jdbc.ZStatefulConnection._

import java.sql.Connection

final class ZStatefulConnection(
  private[jdbc] val underlying: Connection,
  private[jdbc] val dirtyBits: Ref[Int],
  private[jdbc] val defaultTxnIsolationLevel: TransactionIsolationLevel
) {

  def resetState: Task[Unit] =
    for {
      currentDirtyBits <- dirtyBits.get
      _                <- ZIO.when(currentDirtyBits != DirtyBitInitial) {
                            for {
                              _ <- reset(DirtyBitAutoCommit)(_.setAutoCommit(DefaultAutoCommitMode))
                              _ <- reset(DirtyBitTransactionIsolation)(_.setTransactionIsolation(defaultTxnIsolationLevel.value))
                              _ <- dirtyBits.set(DirtyBitInitial)
                            } yield ()
                          }
    } yield ()

  def setAutoCommit(autoCommit: Boolean): Task[Unit] =
    set(DirtyBitAutoCommit)(_.setAutoCommit(autoCommit))

  def setTransactionIsolation(level: TransactionIsolationLevel): Task[Unit] =
    set(DirtyBitTransactionIsolation)(_.setTransactionIsolation(level.value))

  private def reset[A](dirtyBit: Int)(f: Connection => A): Task[Unit] =
    for {
      currentDirtyBits <- dirtyBits.get
      _                <- ZIO.when((currentDirtyBits & dirtyBit) != 0)(ZIO.attemptBlocking(f(underlying)))
    } yield ()

  private def set[A](dirtyBit: Int)(f: Connection => A): Task[Unit] =
    for {
      _ <- ZIO.attemptBlocking(f(underlying))
      _ <- dirtyBits.update(_ | dirtyBit)
    } yield ()

}

object ZStatefulConnection {

  def make(underlying: Connection): Task[ZStatefulConnection] =
    for {
      defaultTxnIsolation      <- ZIO.attemptBlocking(underlying.getTransactionIsolation)
      defaultTxnIsolationLevel <- ZIO.fromEither(TransactionIsolationLevel.fromInt(defaultTxnIsolation))
      dirtyBits                <- Ref.make(DirtyBitInitial)
    } yield new ZStatefulConnection(underlying, dirtyBits, defaultTxnIsolationLevel)

  private[jdbc] val DefaultAutoCommitMode = true

  private[jdbc] val DirtyBitInitial              = 0      // 0b00000000
  private[jdbc] val DirtyBitAutoCommit           = 1      // 0b00000001
  private[jdbc] val DirtyBitTransactionIsolation = 1 << 1 // 0b00000010

}
