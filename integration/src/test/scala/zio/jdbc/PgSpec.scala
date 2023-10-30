package zio.jdbc

import org.testcontainers.containers.{ PostgreSQLContainer, PostgreSQLContainerProvider }
import zio.test.ZIOSpec
import zio.{ ZIO, ZLayer }

abstract class PgSpec extends ZIOSpec[ZConnectionPool] {

  override def bootstrap: ZLayer[Any, Throwable, ZConnectionPool] =
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
          host = "localhost",
          port = pg.getMappedPort(PostgreSQLContainer.POSTGRESQL_PORT),
          database = pg.getDatabaseName,
          props = Map(
            "user"     -> pg.getUsername,
            "password" -> pg.getPassword
          )
        )
    }
}
