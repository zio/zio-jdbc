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

import java.io.File
import java.lang.ClassNotFoundException
import java.sql.{ Connection, SQLException, SQLTimeoutException }

/**
 * A `ZConnectionPool` represents a pool of connections, and has the ability to
 * supply a transaction that can be used for executing SQL statements.
 */
abstract class ZConnectionPool {
  def transaction: ZLayer[Any, ConnectionException, ZConnection]
  def invalidate(conn: ZConnection): UIO[Any]
}

object ZConnectionPool {

  def h2test: ZLayer[Any, ConnectionException, ZConnectionPool] =
    ZLayer.scoped {
      for {
        int  <- Random.nextInt
        zenv <- connect("org.h2.Driver", s"jdbc:h2:mem:test_database_$int", Map.empty).build
      } yield zenv.get[ZConnectionPool]
    }

  def h2mem(
    database: String,
    props: Map[String, String] = Map()
  ): ZLayer[ZConnectionPoolConfig, ConnectionException, ZConnectionPool] =
    connect("org.h2.Driver", s"jdbc:h2:mem:$database", props)

  def h2file(
    directory: File,
    database: String,
    props: Map[String, String] = Map()
  ): ZLayer[ZConnectionPoolConfig, ConnectionException, ZConnectionPool] =
    connect("org.h2.Driver", s"jdbc:h2:file:$directory/$database", props)

  def oracle(
    host: String,
    port: Int,
    database: String,
    props: Map[String, String]
  ): ZLayer[ZConnectionPoolConfig, ConnectionException, ZConnectionPool] =
    connect("oracle.jdbc.OracleDriver", s"jdbc:oracle:thin:@$host:$port:$database", props)

  def postgres(
    host: String,
    port: Int,
    database: String,
    props: Map[String, String]
  ): ZLayer[ZConnectionPoolConfig, ConnectionException, ZConnectionPool] =
    connect("org.postgresql.Driver", s"jdbc:postgresql://$host:$port/$database", props)

  def sqlserver(
    host: String,
    port: Int,
    database: String,
    props: Map[String, String]
  ): ZLayer[ZConnectionPoolConfig, ConnectionException, ZConnectionPool] =
    connect(
      "com.microsoft.sqlserver.jdbc.SQLServerDriver",
      s"jdbc:sqlserver://$host:$port;databaseName=$database",
      props
    )

  def mysql(
    host: String,
    port: Int,
    database: String,
    props: Map[String, String]
  ): ZLayer[ZConnectionPoolConfig, ConnectionException, ZConnectionPool] =
    connect("com.mysql.cj.jdbc.Driver", s"jdbc:mysql://$host:$port/$database", props)

  def connect(
    driverName: String,
    url: String,
    props: Map[String, String]
  ): ZLayer[Any, ConnectionException, ZConnectionPool] =
    ZLayer.scoped {
      for {
        _      <- ZIO.attempt(Class.forName(driverName)).refineOrDie { case e: ClassNotFoundException =>
                    DriverNotFound(e, driverName)
                  }
        acquire = ZIO.attemptBlocking {
                    val properties = new java.util.Properties
                    props.foreach { case (k, v) => properties.setProperty(k, v) }

                    java.sql.DriverManager.getConnection(url, properties)
                  }.refineOrDie {
                    case e: SQLTimeoutException => ConnectionTimeout(e)
                    case e: SQLException        => DBError(e)
                  }
        zenv   <- make(acquire).build.provideSome[Scope](ZLayer.succeed(ZConnectionPoolConfig.default))
      } yield zenv.get[ZConnectionPool]
    }

  def make(
    acquire: IO[ConnectionException, Connection]
  ): ZLayer[ZConnectionPoolConfig, ConnectionException, ZConnectionPool] =
    ZLayer.scoped {
      for {
        config <- ZIO.service[ZConnectionPoolConfig]
        getConn = ZIO.acquireRelease(acquire.retry(config.retryPolicy).flatMap(ZConnection.make))(_.close.ignoreLogged)
        pool   <- ZPool.make(getConn, Range(config.minConnections, config.maxConnections), config.timeToLive)
        tx      = ZLayer.scoped {
                    for {
                      connection <- pool.get
                      _          <- ZIO.addFinalizerExit { exit =>
                                      ZIO
                                        .ifZIO(connection.isValid().orElse(ZIO.succeed(false)))(
                                          onTrue = exit match {
                                            case Exit.Success(_) => connection.restore
                                            case Exit.Failure(_) =>
                                              for {
                                                autoCommitMode <- connection.access(_.getAutoCommit).orElseSucceed(true)
                                                _              <- ZIO.unless(autoCommitMode)(connection.rollback.ignoreLogged)
                                                _              <- connection.restore
                                              } yield ()
                                          },
                                          onFalse = pool.invalidate(connection)
                                        )
                                    }
                    } yield connection
                  }
      } yield new ZConnectionPool {
        def transaction: ZLayer[Any, ConnectionException, ZConnection] = tx
        def invalidate(conn: ZConnection): UIO[Any]                    = pool.invalidate(conn)
      }
    }
}
