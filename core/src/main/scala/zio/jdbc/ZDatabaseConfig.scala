package zio.jdbc

final case class ZDatabaseConfig(
  jdbcUrl: String,
  driver: Option[String],
  user: Option[String],
  password: Option[String],
  properties: Map[String, String]
)
