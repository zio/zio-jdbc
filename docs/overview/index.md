---
id: overview_index
title: "Summary"
---

_ZIO JDBC_ is a ZIO library for JDBC access, providing a small, unopinionated, and ergonomic foundation for directly interacting with JDBC.

_ZIO JDBC_ provides the following components:

 - A SQL interpolator to prevent SQL injection attacks
 - A connection pool powered by ZIO's async, resource-safe pool
 - Lightweight decoding values from result sets 
 - Lightweight encoding values into SQL fragments for `INSERT`
 - Integration with ZIO Schema, ZIO Config, and ZIO Logging
 - Built-in metrics

## Installation

_ZIO JDBC_ is available via Maven so importing in `build.sbt` is sufficient:

```scala
libraryDependencies += "dev.zio" %% "zio-jdbc" % version
```
## Hello World

```scala
import zio.jdbc._

transaction {
  selectOne {
    sql"select name, age where name = 'Sherlock Holmes'".as[(String, Int)]
  }
}
```