package zio.jdbc

import org.testcontainers.containers.PostgreSQLContainer
import org.testcontainers.containers.PostgreSQLContainerProvider
import zio.ZLayer
import zio.ZIO
import zio.test.ZIOSpec

abstract class PgSpec extends ZIOSpec[ZConnectionPool] {
  def bootstrap: ZLayer[Any, Throwable, ZConnectionPool] =
    (ZLayer.scoped {
      ZIO.fromAutoCloseable {
        ZIO.attemptBlocking {
          val provider = new PostgreSQLContainerProvider
          val db       = provider.newInstance()
          db.start()
          db.asInstanceOf[PostgreSQLContainer[Nothing]]
        }
      }
    } ++ ZLayer.succeed(ZConnectionPoolConfig.default)) >>> ZLayer.service[PostgreSQLContainer[Nothing]].flatMap {
      env =>
        val pg = env.get
        ZConnectionPool.postgres(
          "localhost",
          pg.getMappedPort(PostgreSQLContainer.POSTGRESQL_PORT),
          pg.getDatabaseName(),
          Map(
            "user"     -> pg.getUsername(),
            "password" -> pg.getPassword()
          )
        )
    }
}
