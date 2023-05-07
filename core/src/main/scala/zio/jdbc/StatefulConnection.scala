package zio.jdbc

import zio._
import zio.jdbc.StatefulConnection._

import java.sql.Connection

final class StatefulConnection(
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

object StatefulConnection {

  def make(underlying: Connection): Task[StatefulConnection] =
    for {
      defaultTxnIsolation      <- ZIO.attemptBlocking(underlying.getTransactionIsolation)
      defaultTxnIsolationLevel <- ZIO.fromEither(TransactionIsolationLevel.fromInt(defaultTxnIsolation))
      dirtyBits                <- Ref.make(DirtyBitInitial)
    } yield new StatefulConnection(underlying, dirtyBits, defaultTxnIsolationLevel)

  private[jdbc] val DefaultAutoCommitMode = true

  private[jdbc] val DirtyBitInitial              = 0      // 0b00000000
  private[jdbc] val DirtyBitAutoCommit           = 1      // 0b00000001
  private[jdbc] val DirtyBitTransactionIsolation = 1 << 1 // 0b00000010

}
