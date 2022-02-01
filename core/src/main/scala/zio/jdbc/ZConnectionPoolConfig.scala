package zio.jdbc

import zio._

/**
 * Configuration data for a connection pool.
 *
 * TODO: Make ZIO Config ConfigDescriptor for this data type.
 */
final case class ZConnectionPoolConfig(
  minConnections: Int,
  maxConnections: Int,
  retryPolicy: Schedule[Any, Throwable, Any],
  timeToLive: Duration
)
