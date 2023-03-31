---
id: installation
title: "Installation"
---

## Installation

_ZIO JDBC_ is available via Maven so importing in `build.sbt` is sufficient:

```scala
libraryDependencies += "dev.zio" %% "zio-jdbc" % "@VERSION@"
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
