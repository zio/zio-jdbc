package zio.jdbc

import zio._

final case class ZConnectionPoolConfig(
  minConnections: Int,
  maxConnections: Int,
  retryPolicy: Schedule[Any, Throwable, Any],
  timeToLive: Duration
)
