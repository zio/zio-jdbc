import BuildHelper._

val ZioVersion        = "2.0.0"
val H2Version         = "2.1.210"
val ZioConfigVersion  = "3.0.1"
val ZioLoggingVersion = "2.0.0"
val ZioSchemaVersion  = "0.2.0"

name := "zio-jdbc"

inThisBuild(
  List(
    organization := "dev.zio",
    homepage     := Some(url("https://zio.github.io/zio-jdbc/")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers   := List(
      Developer("jdegoes", "John De Goes", "john@degoes.net", url("http://degoes.net"))
    )
  )
)

addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("fmt", "; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll")

addCommandAlias(
  "testJVM",
  ";core/test"
)

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true
  )
  .aggregate(core, docs, examples)

lazy val core = project
  .in(file("core"))
  .settings(stdSettings("zio-jdbc"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio"          % ZioVersion,
      "dev.zio"       %% "zio-streams"  % ZioVersion,
      "dev.zio"       %% "zio-config"   % ZioConfigVersion,
      "dev.zio"       %% "zio-logging"  % ZioLoggingVersion,
      "dev.zio"       %% "zio-schema"   % ZioSchemaVersion,
      "dev.zio"       %% "zio-test"     % ZioVersion % Test,
      "dev.zio"       %% "zio-test-sbt" % ZioVersion % Test,
      "com.h2database" % "h2"           % H2Version  % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    Test / fork    := true,
    run / fork     := true
  )

lazy val docs = project
  .in(file("zio-jdbc-docs"))
  .settings(stdSettings("zio-jdbc-docs"))
  .settings(
    publish / skip                             := true,
    moduleName                                 := "zio-jdbc-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(core),
    ScalaUnidoc / unidoc / target              := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite                       := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages                   := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .dependsOn(core)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val examples = project
  .in(file("examples"))
  .dependsOn(core)
  .settings(stdSettings("zio-jdbc-examples"))
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "ch.qos.logback"       % "logback-classic"          % "1.2.6",
      "net.logstash.logback" % "logstash-logback-encoder" % "6.6"
    )
  )
