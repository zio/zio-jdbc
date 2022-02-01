package zio.jdbc

import zio._
import java.sql.Connection
import java.io.File

final case class ZConnectionPool(transaction: ZLayer[Any, Throwable, ZConnection])
object ZConnectionPool {
  def h2mem(
    database: String,
    props: Map[String, String] = Map()
  ): ZLayer[Clock & ZConnectionPoolConfig, Throwable, ZConnectionPool] =
    ZLayer {
      for {
        _      <- ZManaged.attempt(Class.forName("org.h2.Driver"))
        config <- ZManaged.service[ZConnectionPoolConfig]
        acquire = Task.attemptBlocking {
                    val properties = new java.util.Properties
                    props.foreach { case (k, v) => properties.setProperty(k, v) }

                    java.sql.DriverManager.getConnection(s"jdbc:h2:mem:$database", properties)
                  }
        zenv   <- make(acquire).build
      } yield zenv.get[ZConnectionPool]
    }

  def h2file(
    directory: File,
    database: String,
    props: Map[String, String] = Map()
  ): ZLayer[Clock & ZConnectionPoolConfig, Throwable, ZConnectionPool] =
    ZLayer {
      for {
        _      <- ZManaged.attempt(Class.forName("org.h2.Driver"))
        config <- ZManaged.service[ZConnectionPoolConfig]
        acquire = Task.attemptBlocking {
                    val properties = new java.util.Properties
                    props.foreach { case (k, v) => properties.setProperty(k, v) }

                    java.sql.DriverManager.getConnection(s"jdbc:h2:file:$directory/$database", properties)
                  }
        zenv   <- make(acquire).build
      } yield zenv.get[ZConnectionPool]
    }

  def oracle(
    host: String,
    port: Int,
    database: String,
    props: Map[String, String]
  ): ZLayer[Clock & ZConnectionPoolConfig, Throwable, ZConnectionPool] =
    ZLayer {
      for {
        _      <- ZManaged.attempt(Class.forName("oracle.jdbc.OracleDriver"))
        config <- ZManaged.service[ZConnectionPoolConfig]
        acquire = Task.attemptBlocking {
                    val properties = new java.util.Properties
                    props.foreach { case (k, v) => properties.setProperty(k, v) }

                    java.sql.DriverManager.getConnection(s"jdbc:oracle:thin:@$host:$port:$database", properties)
                  }
        zenv   <- make(acquire).build
      } yield zenv.get[ZConnectionPool]
    }

  def postgres(
    host: String,
    port: Int,
    database: String,
    props: Map[String, String]
  ): ZLayer[Clock & ZConnectionPoolConfig, Throwable, ZConnectionPool] =
    ZLayer {
      for {
        _      <- ZManaged.attempt(Class.forName("org.postgresql.Driver"))
        config <- ZManaged.service[ZConnectionPoolConfig]
        acquire = Task.attemptBlocking {
                    val properties = new java.util.Properties

                    props.foreach { case (k, v) => properties.setProperty(k, v) }

                    java.sql.DriverManager.getConnection(s"jdbc:postgresql://$host:$port/$database", properties)
                  }
        zenv   <- make(acquire).build
      } yield zenv.get[ZConnectionPool]
    }

  def sqlserver(
    host: String,
    port: Int,
    database: String,
    props: Map[String, String]
  ): ZLayer[Clock & ZConnectionPoolConfig, Throwable, ZConnectionPool] =
    ZLayer {
      for {
        _      <- ZManaged.attempt(Class.forName("com.microsoft.sqlserver.jdbc.SQLServerDriver"))
        config <- ZManaged.service[ZConnectionPoolConfig]
        acquire = Task.attemptBlocking {
                    val properties = new java.util.Properties

                    props.foreach { case (k, v) => properties.setProperty(k, v) }

                    java.sql.DriverManager
                      .getConnection(s"jdbc:sqlserver://$host:$port;databaseName=$database", properties)
                  }
        zenv   <- make(acquire).build
      } yield zenv.get[ZConnectionPool]
    }

  def make(acquire: Task[Connection]): ZLayer[Clock & ZConnectionPoolConfig, Throwable, ZConnectionPool] =
    ZLayer {
      for {
        config <- ZManaged.service[ZConnectionPoolConfig]
        clock  <- ZManaged.service[Clock]
        managed = ZManaged.acquireReleaseWith(acquire.retry(config.retryPolicy))(conn => UIO(conn.close()))
        pool   <-
          ZPool
            .make(managed.map(ZConnection(_)), Range(config.minConnections, config.maxConnections), config.timeToLive)
            .provide(ZLayer.succeed(clock))
      } yield ZConnectionPool {
        ZLayer {
          for {
            connection <- pool.get
            _          <- ZManaged.finalizerExit {
                            case Exit.Success(_) => UIO.unit
                            case Exit.Failure(_) => UIO(connection.connection.rollback())
                          }
          } yield connection
        }
      }
    }
}
