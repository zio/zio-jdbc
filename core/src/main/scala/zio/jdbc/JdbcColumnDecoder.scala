package zio.jdbc

import java.sql.ResultSet
import java.sql.Blob

trait JdbcColumnDecoder[+A] {
  def decode(column: Int, resultSet: ResultSet): A
}
object JdbcColumnDecoder    {
  def apply[A](f: (Int, ResultSet) => A): JdbcColumnDecoder[A] =
    new JdbcColumnDecoder[A] {
      def decode(column: Int, resultSet: ResultSet): A = f(column, resultSet)
    }

  implicit val stringDecoder: JdbcColumnDecoder[String] =
    JdbcColumnDecoder((column, resultSet) => resultSet.getString(column))

  implicit val intDecoder: JdbcColumnDecoder[Int] =
    JdbcColumnDecoder((column, resultSet) => resultSet.getInt(column))

  implicit val longDecoder: JdbcColumnDecoder[Long] =
    JdbcColumnDecoder((column, resultSet) => resultSet.getLong(column))

  implicit val doubleDecoder: JdbcColumnDecoder[Double] =
    JdbcColumnDecoder((column, resultSet) => resultSet.getDouble(column))

  implicit val floatDecoder: JdbcColumnDecoder[Float] =
    JdbcColumnDecoder((column, resultSet) => resultSet.getFloat(column))

  implicit val shortDecoder: JdbcColumnDecoder[Short] =
    JdbcColumnDecoder((column, resultSet) => resultSet.getShort(column))

  implicit val byteDecoder: JdbcColumnDecoder[Byte] =
    JdbcColumnDecoder((column, resultSet) => resultSet.getByte(column))

  implicit val booleanDecoder: JdbcColumnDecoder[Boolean] =
    JdbcColumnDecoder((column, resultSet) => resultSet.getBoolean(column))

  implicit val bigDecimalDecoder: JdbcColumnDecoder[java.math.BigDecimal] =
    JdbcColumnDecoder((column, resultSet) => resultSet.getBigDecimal(column))

  implicit val bigDecimalDecoderScala: JdbcColumnDecoder[BigDecimal] =
    JdbcColumnDecoder((column, resultSet) => resultSet.getBigDecimal(column))

  implicit val blobDecoder: JdbcColumnDecoder[Blob] =
    JdbcColumnDecoder((column, resultSet) => resultSet.getBlob(column))

  implicit def optionDecoder[A](implicit decoder: JdbcColumnDecoder[A]): JdbcColumnDecoder[Option[A]] =
    JdbcColumnDecoder((column, resultSet) =>
      if (resultSet.getObject(column) == null) None
      else Some(decoder.decode(column, resultSet))
    )
}
