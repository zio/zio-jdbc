package zio.jdbc

import org.testcontainers.containers.{ PostgreSQLContainer, PostgreSQLContainerProvider }
import zio.test.ZIOSpec
import zio.{ ZIO, ZLayer }

import java.util.TimeZone

abstract class PgSpec extends ZIOSpec[ZConnectionPool] {

  /**
   * We need to set the JVM default timezone to UTC before doing anything else
   * because this default time zone will be picked up by JDBC (See [[org.postgresql.jdbc.TimestampUtils.getDefaultTz]]
   * and will be used to convert timestamps to UTC before to send them to the DB.
   * When the computer on which tests are running is using UTC (like in CI, for example), that'll not change anything.
   * When the computer on which tests are running is using another timezone, the `java.time.*` tests will fail because the saved timestamps will be converted to UTC from the computer timezone.
   * So, we need to set the VM default timezone to UTC so that the tests are executed in the exact same way and produce the exact same results in the CI or on your local computer.
   */
  override def bootstrap: ZLayer[Any, Throwable, ZConnectionPool] =
    ZLayer.succeed(TimeZone.setDefault(TimeZone.getTimeZone("UTC"))) >>>
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
