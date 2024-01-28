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

/**
 * Configuration data for a connection pool.
 */
final case class ZConnectionPoolConfig(
  minConnections: Int,
  maxConnections: Int,
  retryPolicy: Schedule[Any, ConnectionException, Any],
  timeToLive: Duration
)
object ZConnectionPoolConfig {
  import zio.schema._
  import Schema.Field

  lazy val default: ZConnectionPoolConfig = ZConnectionPoolConfig(8, 32, defaultRetryPolicy, 300.seconds)

  lazy val defaultRetryPolicy: Schedule.WithState[(Long, Long), Any, Any, (Long, Duration)] =
    Schedule.recurs(10) && Schedule.exponential(15.millis)

  implicit val config: Config[ZConnectionPoolConfig] =
    (Config.int("minConnections") zip
      Config.int("maxConnections") zip
      Config.duration("timeToLive")).map { case (min, max, ttl) =>
      ZConnectionPoolConfig(min, max, defaultRetryPolicy, ttl)
    }

  implicit val schema: Schema.CaseClass3[Int, Int, Duration, ZConnectionPoolConfig] =
    Schema.CaseClass3[Int, Int, Duration, ZConnectionPoolConfig](
      TypeId.parse(classOf[ZConnectionPoolConfig].getName),
      Field(
        "minConnections",
        Schema[Int],
        get0 = _.minConnections,
        set0 = (c, x) => c.copy(minConnections = x)
      ),
      Field(
        "maxConnections",
        Schema[Int],
        get0 = _.maxConnections,
        set0 = (c, x) => c.copy(maxConnections = x)
      ),
      Field(
        "timeToLive",
        Schema.Primitive(StandardType.DurationType),
        get0 = _.timeToLive,
        set0 = (c, x) => c.copy(timeToLive = x)
      ),
      (min, max, ttl) => ZConnectionPoolConfig(min, max, defaultRetryPolicy, ttl)
    )
}
