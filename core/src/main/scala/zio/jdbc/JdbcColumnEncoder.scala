package zio.jdbc

import java.sql.PreparedStatement
import java.sql.Blob

trait JdbcColumnEncoder[-A]    {
  def encode(column: Int, value: A, statement: PreparedStatement): Unit
}
object ResuletSetColumnEncoder {
  implicit val stringEncoder: JdbcColumnEncoder[String] =
    (column, value, statement) => statement.setString(column, value)

  implicit val intEncoder: JdbcColumnEncoder[Int] =
    (column, value, statement) => statement.setInt(column, value)

  implicit val longEncoder: JdbcColumnEncoder[Long] =
    (column, value, statement) => statement.setLong(column, value)

  implicit val doubleEncoder: JdbcColumnEncoder[Double] =
    (column, value, statement) => statement.setDouble(column, value)

  implicit val floatEncoder: JdbcColumnEncoder[Float] =
    (column, value, statement) => statement.setFloat(column, value)

  implicit val shortEncoder: JdbcColumnEncoder[Short] =
    (column, value, statement) => statement.setShort(column, value)

  implicit val byteEncoder: JdbcColumnEncoder[Byte] =
    (column, value, statement) => statement.setByte(column, value)

  implicit val booleanEncoder: JdbcColumnEncoder[Boolean] =
    (column, value, statement) => statement.setBoolean(column, value)

  implicit val bigDecimalEncoder: JdbcColumnEncoder[java.math.BigDecimal] =
    (column, value, statement) => statement.setBigDecimal(column, value)

  implicit val bigDecimalEncoderScala: JdbcColumnEncoder[BigDecimal] =
    (column, value, statement) => statement.setBigDecimal(column, value.bigDecimal)

  implicit val blobEncoder: JdbcColumnEncoder[Blob] =
    (column, value, statement) => statement.setBlob(column, value)

  implicit def optionEncoder[A](implicit encoder: JdbcColumnEncoder[A]): JdbcColumnEncoder[Option[A]] =
    (column, value, statement) => value.foreach(a => encoder.encode(column, a, statement))
}
