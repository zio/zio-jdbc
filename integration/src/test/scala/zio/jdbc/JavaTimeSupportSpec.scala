package zio.jdbc

import zio.Scope
import zio.test._

object JavaTimeSupportSpec extends PgSpec {

  /**
   * Copied from Quill
   */
  final case class TimeEntity(
    sqlDate: java.sql.Date,                      // DATE
    sqlTime: java.sql.Time,                      // TIME
    sqlTimestamp: java.sql.Timestamp,            // TIMESTAMP
    timeLocalDate: java.time.LocalDate,          // DATE
    timeLocalTime: java.time.LocalTime,          // TIME
    timeLocalDateTime: java.time.LocalDateTime,  // TIMESTAMP
    timeZonedDateTime: java.time.ZonedDateTime,  // TIMESTAMP_WITH_TIMEZONE
    timeInstant: java.time.Instant,              // TIMESTAMP
    timeOffsetTime: java.time.OffsetTime,        // TIME_WITH_TIMEZONE
    timeOffsetDateTime: java.time.OffsetDateTime // TIMESTAMP_WITH_TIMEZONE
  )
  object TimeEntity {
    implicit val jdbcDecoder: JdbcDecoder[TimeEntity] =
      JdbcDecoder[
        (
          java.sql.Date,
          java.sql.Time,
          java.sql.Timestamp,
          java.time.LocalDate,
          java.time.LocalTime,
          java.time.LocalDateTime,
          java.time.ZonedDateTime,
          java.time.Instant,
          java.time.OffsetTime,
          java.time.OffsetDateTime
        )
      ].map((TimeEntity.apply _).tupled)

    implicit val jdbcEncoder: JdbcEncoder[TimeEntity] =
      JdbcEncoder[
        (
          java.sql.Date,
          java.sql.Time,
          java.sql.Timestamp,
          java.time.LocalDate,
          java.time.LocalTime,
          java.time.LocalDateTime,
          java.time.ZonedDateTime,
          java.time.Instant,
          java.time.OffsetTime,
          java.time.OffsetDateTime
        )
      ].contramap(TimeEntity.unapply(_).get)
  }

  val genTimeEntity: Gen[Any, TimeEntity] =
    for {
      sqlDate            <- Gen.localDate.map(java.sql.Date.valueOf)
      sqlTime            <- Gen.localTime.map(java.sql.Time.valueOf)
      sqlTimestamp       <- Gen.localDateTime.map(java.sql.Timestamp.valueOf)
      timeLocalDate      <- Gen.localDate
      timeLocalTime      <- Gen.localTime
      timeLocalDateTime  <- Gen.localDateTime
      timeZonedDateTime  <- Gen.zonedDateTime
      timeInstant        <- Gen.instant
      timeOffsetTime     <- Gen.offsetTime
      timeOffsetDateTime <- Gen.offsetDateTime
    } yield TimeEntity(
      sqlDate = sqlDate,
      sqlTime = sqlTime,
      sqlTimestamp = sqlTimestamp,
      timeLocalDate = timeLocalDate,
      timeLocalTime = timeLocalTime,
      timeLocalDateTime = timeLocalDateTime,
      timeZonedDateTime = timeZonedDateTime,
      timeInstant = timeInstant,
      timeOffsetTime = timeOffsetTime,
      timeOffsetDateTime = timeOffsetDateTime
    )

  override def spec: Spec[ZConnectionPool with TestEnvironment with Scope, Any] =
    suite("java.time.* types support")(
      test("insert and select") {
        check(genTimeEntity) { timeEntity =>
          for {
            insertResult <- transaction {
                              sql"""INSERT INTO time (
                                sqlDate,
                                sqlTime,
                                sqlTimestamp,
                                timeLocalDate,
                                timeLocalTime,
                                timeLocalDateTime,
                                timeZonedDateTime,
                                timeInstant,
                                timeOffsetTime,
                                timeOffsetDateTime
                              ) VALUES (
                                ${timeEntity.sqlDate},
                                ${timeEntity.sqlTime},
                                ${timeEntity.sqlTimestamp},
                                ${timeEntity.timeLocalDate},
                                ${timeEntity.timeLocalTime},
                                ${timeEntity.timeLocalDateTime},
                                ${timeEntity.timeZonedDateTime},
                                ${timeEntity.timeInstant},
                                ${timeEntity.timeOffsetTime},
                                ${timeEntity.timeOffsetDateTime}
                              )""".insert
                            }
            selectResult <- transaction(sql"SELECT * FROM time".query[TimeEntity].selectOne)
          } yield assertTrue(
            insertResult == 1L,
            selectResult.isDefined,
            selectResult.get == timeEntity
          )
        }
      }
    ) @@ TestAspect.sequential @@ TestAspect.around(
      before = transaction(
        sql"""CREATE TABLE time (
                id SERIAL,
                sqlDate DATE,
                sqlTime TIME,
                sqlTimestamp TIMESTAMP,
                timeLocalDate DATE,
                timeLocalTime TIME,
                timeLocalDateTime TIMESTAMP,
                timeZonedDateTime TIMESTAMPTZ,
                timeInstant TIMESTAMP,
                timeOffsetTime TIMETZ,
                timeOffsetDateTime TIMESTAMPTZ
              )""".execute
      ),
      after = transaction(sql"DROP TABLE time".execute).orDie
    )
}
