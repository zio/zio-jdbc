# ZIO JDBC

| Project Stage | CI                                       | Release                                                               |  Issues                                                     | Discord                                   |
| --- |------------------------------------------|-----------------------------------------------------------------------|--------------------------------------------------------------|-------------------------------------------|
| [![Project stage][Stage]][Stage-Page] | ![CI][badge-CI] | [![Release Artifacts][badge-sonatype-releases]][link-sonatype-releases] | [![Is it maintained?][badge-maintenance]][link-maintenance] | [![Discord][badge-discord]][link-discord] |

_ZIO JDBC_ is a small, idiomatic ZIO interface to JDBC, providing a pleasant and developer-friendly experience to low-level JDBC access.

- Idiomatic ZIO 2.0 interface to JDBC
- Secure, with protection against SQL-injection
- Fully integrated with core libraries including _ZIO Schema_, _ZIO Config_, _ZIO Logging_

## Example

```scala
// Creating SQL statements using interpolation:
val ex1 = sql"select * from users where age = $age"

// Selecting into tuples:
val ex2 = sql"select name, age from users".as[(String, Int)]

// Inserting from tuples:
val ex3 = sql"insert into users ('name', 'age')".values(("John", 42))

// Executing statements:
val res1: ZIO[ZConnectionPool, Throwable, Option[(String, Int)]] =
  transaction {
    selectOne(sql"select name, age from users where name = 'Sherlock Holmes'".as[(String, Int)])
  }
```

To learn more about _ZIO JDBC_, check out the following references:

- [Homepage](https://zio.github.io/zio-jdbc/)
- [Contributor's guide](./.github/CONTRIBUTING.md)
- [License](LICENSE)
- [Issues](https://github.com/zio/zio-jdbc/issues)
- [Pull Requests](https://github.com/zio/zio-jdbc/pulls)

[badge-sonatype-releases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-jdbc_2.12.svg "Sonatype Releases"
[badge-CI]: https://github.com/zio/zio-jdbc/workflows/CI/badge.svg
[badge-discord]: https://img.shields.io/discord/629491597070827530?logo=discord
[badge-maintenance]: http://isitmaintained.com/badge/resolution/zio/zio-jdbc.svg
[link-sonatype-releases]: https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-jdbc_2.12/ "Sonatype Releases"
[link-discord]: https://discord.gg/2ccFBr4
[link-maintenance]: http://isitmaintained.com/project/zio/zio-jdbc
[link-zio]: https://zio.dev
[Stage]: https://img.shields.io/badge/Project%20Stage-Research-red.svg
[Stage-Page]: https://github.com/zio/zio/wiki/Project-Stages
