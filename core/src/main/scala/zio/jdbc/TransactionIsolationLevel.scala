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
