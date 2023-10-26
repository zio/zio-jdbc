package zio.jdbc

import zio.test.TestAspect._
import zio.test._
import zio.{ Scope, ZIO }

import java.time._
import java.time.chrono.IsoEra
import java.time.temporal.{ ChronoField, ChronoUnit }

object JavaTimeSupportSpec extends PgSpec {

  /**
   * Constants copied from https://github.com/pgjdbc/pgjdbc/blob/REL42.6.0/pgjdbc/src/main/java/org/postgresql/jdbc/TimestampUtils.java#L59-L67
   *
   * See also https://www.postgresql.org/docs/current/datatype-datetime.html
   */
  object PgConstants {
    // LocalTime.MAX is 23:59:59.999_999_999, and it wraps to 24:00:00 when nanos exceed 999_999_499
    // since PostgreSQL has microsecond resolution only
    val MAX_TIME: LocalTime                 = LocalTime.MAX.minus(Duration.ofNanos(500))
    // low value for dates is   4713 BC
    val MIN_LOCAL_DATE: LocalDate           = LocalDate.of(4713, 1, 1).`with`(ChronoField.ERA, IsoEra.BCE.getValue)
    val MIN_LOCAL_DATETIME: LocalDateTime   = MIN_LOCAL_DATE.atStartOfDay
    val MIN_OFFSET_DATETIME: OffsetDateTime = MIN_LOCAL_DATETIME.atOffset(ZoneOffset.UTC)
  }
  import PgConstants._

  /**
   * We didn't find these constants in the pgjdbc code or because the one we found are not the ones indicated in the doc.
   * See https://www.postgresql.org/docs/current/datatype-datetime.html
   */
  object ManuallyWrittenPgConstants {
    val MAX_LOCAL_DATETIME: LocalDateTime =
      LocalDate
        .of(294276, 1, 1)
        .`with`(ChronoField.ERA, IsoEra.CE.getValue)
        .atStartOfDay
        .minus(500, ChronoUnit.NANOS)

    val MAX_OFFSET_DATETIME: OffsetDateTime =
      LocalDate
        .of(294276, 1, 1)
        .`with`(ChronoField.ERA, IsoEra.CE.getValue)
        .atTime(OffsetTime.MAX)
        .minus(500, ChronoUnit.NANOS)

    val MAX_LOCAL_DATE: LocalDate =
      LocalDate
        .of(5874897, 1, 1)
        .`with`(ChronoField.ERA, IsoEra.CE.getValue)
        .minus(1L, ChronoUnit.DAYS)

    val MIN_TIMESTAMP: Instant = MIN_LOCAL_DATETIME.toInstant(ZoneOffset.UTC)
    val MAX_TIMESTAMP: Instant = MAX_LOCAL_DATETIME.toInstant(ZoneOffset.UTC)
  }
  import ManuallyWrittenPgConstants._

  val genPGLocalDate: Gen[Any, LocalDate]           = Gen.localDate(MIN_LOCAL_DATE, MAX_LOCAL_DATE)
  val genPGLocalTime: Gen[Any, LocalTime]           = Gen.localTime(LocalTime.MIN, MAX_TIME)
  val genPGLocalDateTime: Gen[Any, LocalDateTime]   = Gen.localDateTime(MIN_LOCAL_DATETIME, MAX_LOCAL_DATETIME)
  // We need to set `UTC` as PG will move the date to UTC and so can generate a date that is not in the range of `MIN_TIMESTAMP` and `MAX_TIMESTAMP`
  val genPGOffsetDateTime: Gen[Any, OffsetDateTime] =
    Gen.offsetDateTime(MIN_OFFSET_DATETIME, MAX_OFFSET_DATETIME).map(_.withOffsetSameInstant(ZoneOffset.UTC))
  val genPGInstant: Gen[Any, Instant]               = Gen.instant(MIN_TIMESTAMP, MAX_TIMESTAMP)

  /**
   * Adapted from [[Gen.zonedDateTime]]
   */
  val genPGZonedDateTime: Gen[Any, ZonedDateTime] =
    for {
      offsetDateTime <- genPGOffsetDateTime
      zoneId         <- Gen.zoneId
    } yield offsetDateTime.atZoneSameInstant(zoneId)

  val genSqlDate: Gen[Any, java.sql.Date]           = genPGLocalDate.map(java.sql.Date.valueOf)
  val genSqlTime: Gen[Any, java.sql.Time]           = genPGLocalTime.map(java.sql.Time.valueOf)
  val genSqlTimestamp: Gen[Any, java.sql.Timestamp] = genPGInstant.map(java.sql.Timestamp.from)

  /**
   * PG or the PG driver has only a MICRO precision and it rounds up the nanos to the nearest micro
   * For example, 145513948 will be rounded to 145514
   * So I do the same thing here.
   * The math formula comes from ChatGPT
   */
  def nanosRoundedUpToMicros(nanos: Int): Long = (math.round(nanos.toDouble / 1000) * 1000) / 1000

  override def spec: Spec[ZConnectionPool with TestEnvironment with Scope, Any] =
    suite("java.time.* types support")(
      test("java.sql.Date") {
        check(genSqlDate) { sqlDate =>
          for {
            _ <- transaction(sql"""CREATE TABLE sql_date (value DATE)""".execute)
            i <- transaction(sql"""INSERT INTO sql_date VALUES ($sqlDate)""".insert)
            d <- transaction(sql"""SELECT * FROM sql_date""".query[java.sql.Date].selectOne)
            _ <- transaction(sql"DROP TABLE sql_date".execute)
          } yield assertTrue(
            i == 1L,
            d.isDefined,
            d.get == sqlDate
          )
        }
      },
      test("java.sql.Time") {
        check(genSqlTime) { sqlTime =>
          for {
            _ <- transaction(sql"""CREATE TABLE sql_time (value TIME)""".execute)
            i <- transaction(sql"""INSERT INTO sql_time VALUES ($sqlTime)""".insert)
            d <- transaction(sql"""SELECT * FROM sql_time""".query[java.sql.Time].selectOne)
            _ <- transaction(sql"DROP TABLE sql_time".execute)
          } yield assertTrue(
            i == 1L,
            d.isDefined,
            d.get == sqlTime
          )
        }
      },
      test("java.sql.Timestamp - now") {
        for {
          now <- ZIO.clockWith(_.instant).map(java.sql.Timestamp.from)
          _   <- transaction(sql"""CREATE TABLE sql_timestamp (value TIMESTAMP)""".execute)
          i   <- transaction(sql"""INSERT INTO sql_timestamp VALUES ($now)""".insert)
          d   <- transaction(sql"""SELECT * FROM sql_timestamp""".query[java.sql.Timestamp].selectOne)
          _   <- transaction(sql"DROP TABLE sql_timestamp".execute)
        } yield assertTrue(
          i == 1L,
          d.isDefined,
          d.get == now
        )
      },
      test("java.sql.Timestamp - Gen") {
        check(genSqlTimestamp) { sqlTimestamp =>
          for {
            _ <- transaction(sql"""CREATE TABLE sql_timestamp (value TIMESTAMP)""".execute)
            i <- transaction(sql"""INSERT INTO sql_timestamp VALUES ($sqlTimestamp)""".insert)
            d <- transaction(sql"""SELECT * FROM sql_timestamp""".query[java.sql.Timestamp].selectOne)
            _ <- transaction(sql"DROP TABLE sql_timestamp".execute)
            rounded = nanosRoundedUpToMicros(sqlTimestamp.getNanos)
            expected = sqlTimestamp
                         .toInstant
                         .`with`(ChronoField.MICRO_OF_SECOND, rounded) // Replaces the micros with the rounded value
                         .truncatedTo(ChronoUnit.MICROS)
          } yield assertTrue(
            i == 1L,
            d.isDefined,
            d.get == java.sql.Timestamp.from(expected)
          )
        }
      },
      test("java.time.LocalDate") {
        check(genPGLocalDate) { localDate =>
          for {
            _ <- transaction(sql"""CREATE TABLE local_date (value DATE)""".execute)
            i <- transaction(sql"""INSERT INTO local_date VALUES ($localDate)""".insert)
            d <- transaction(sql"""SELECT * FROM local_date""".query[java.time.LocalDate].selectOne)
            _ <- transaction(sql"DROP TABLE local_date".execute)
          } yield assertTrue(
            i == 1L,
            d.isDefined,
            d.get == localDate
          )
        }
      },
      test("java.time.LocalTime") {
        check(genPGLocalTime) { localTime =>
          for {
            _ <- transaction(sql"""CREATE TABLE local_time (value TIME)""".execute)
            i <- transaction(sql"""INSERT INTO local_time VALUES ($localTime)""".insert)
            d <- transaction(sql"""SELECT * FROM local_time""".query[java.time.LocalTime].selectOne)
            _ <- transaction(sql"DROP TABLE local_time".execute)
          } yield assertTrue(
            i == 1L,
            d.isDefined,
            d.get == localTime.truncatedTo(ChronoUnit.SECONDS)
          )
        }
      },
      test("java.time.LocalDateTime") {
        check(genPGLocalDateTime) { localDateTime =>
          for {
            _       <- transaction(sql"""CREATE TABLE local_datetime (value TIMESTAMP)""".execute)
            i       <- transaction(sql"""INSERT INTO local_datetime VALUES ($localDateTime)""".insert)
            d       <- transaction(sql"""SELECT * FROM local_datetime""".query[java.time.LocalDateTime].selectOne)
            _       <- transaction(sql"DROP TABLE local_datetime".execute)
            rounded  = nanosRoundedUpToMicros(localDateTime.getNano)
            expected = localDateTime
                         .`with`(ChronoField.MICRO_OF_SECOND, rounded) // Replaces the micros with the rounded value
                         .truncatedTo(ChronoUnit.MICROS)
          } yield assertTrue(
            i == 1L,
            d.isDefined,
            d.get == expected
          )
        }
      },
      test("java.time.ZonedDateTime") {
        check(genPGZonedDateTime) { zonedDateTime =>
          for {
            _       <- transaction(sql"""CREATE TABLE zoned_datetime (value TIMESTAMP WITH TIME ZONE)""".execute)
            i       <- transaction(sql"""INSERT INTO zoned_datetime VALUES ($zonedDateTime)""".insert)
            d       <- transaction(sql"""SELECT * FROM zoned_datetime""".query[java.time.ZonedDateTime].selectOne)
            _       <- transaction(sql"DROP TABLE zoned_datetime".execute)
            rounded  = nanosRoundedUpToMicros(zonedDateTime.getNano)
            expected = zonedDateTime
                         .`with`(ChronoField.MICRO_OF_SECOND, rounded) // Replaces the micros with the rounded value
                         .truncatedTo(ChronoUnit.MICROS)
                         .withZoneSameInstant(ZoneOffset.UTC)
          } yield assertTrue(
            i == 1L,
            d.isDefined,
            d.get == expected
          )
        }
      },
      test("java.time.Instant - now") {
        for {
          now <- ZIO.clockWith(_.instant)
          _   <- transaction(sql"""CREATE TABLE instant (value TIMESTAMP)""".execute)
          i   <- transaction(sql"""INSERT INTO instant VALUES ($now)""".insert)
          d   <- transaction(sql"""SELECT * FROM instant""".query[java.time.Instant].selectOne)
          _   <- transaction(sql"DROP TABLE instant".execute)
        } yield assertTrue(
          i == 1L,
          d.isDefined,
          d.get == now
        )
      },
      test("java.time.Instant - Gen") {
        check(genPGInstant) { instant =>
          for {
            _ <- transaction(sql"""CREATE TABLE instant (value TIMESTAMP)""".execute)
            i <- transaction(sql"""INSERT INTO instant VALUES ($instant)""".insert)
            d <- transaction(sql"""SELECT * FROM instant""".query[java.time.Instant].selectOne)
            _ <- transaction(sql"DROP TABLE instant".execute)
            rounded = nanosRoundedUpToMicros(instant.getNano)
            expected = instant
                         .`with`(ChronoField.MICRO_OF_SECOND, rounded) // Replaces the micros with the rounded value
                         .truncatedTo(ChronoUnit.MICROS)
          } yield assertTrue(
            i == 1L,
            d.isDefined,
            d.get == expected
          )
        }
      },
      test("java.time.OffsetDateTime") {
        check(genPGOffsetDateTime) { offsetDateTime =>
          for {
            _       <- transaction(sql"""CREATE TABLE offset_datetime (value TIMESTAMP WITH TIME ZONE)""".execute)
            i       <- transaction(sql"""INSERT INTO offset_datetime VALUES ($offsetDateTime)""".insert)
            d       <- transaction(sql"""SELECT * FROM offset_datetime""".query[java.time.OffsetDateTime].selectOne)
            _       <- transaction(sql"DROP TABLE offset_datetime".execute)
            rounded  = nanosRoundedUpToMicros(offsetDateTime.getNano)
            expected = offsetDateTime
                         .`with`(ChronoField.MICRO_OF_SECOND, rounded) // Replaces the micros with the rounded value
                         .truncatedTo(ChronoUnit.MICROS)
                         .withOffsetSameInstant(ZoneOffset.UTC)
          } yield assertTrue(
            i == 1L,
            d.isDefined,
            d.get == expected
          )
        }
      }
    ) @@ sequential @@ shrinks(0) @@ repeats(100) @@ withLiveClock
}
