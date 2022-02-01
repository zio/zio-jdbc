package zio.jdbc

import java.sql.ResultSet
import java.sql.Blob

trait ResultSetColumnDecoder[+A] {
  def decode(column: Int, resultSet: ResultSet): A
}
object ResultSetColumnDecoder    {
  def apply[A](f: (Int, ResultSet) => A): ResultSetColumnDecoder[A] =
    new ResultSetColumnDecoder[A] {
      def decode(column: Int, resultSet: ResultSet): A = f(column, resultSet)
    }

  implicit val stringDecoder: ResultSetColumnDecoder[String] =
    ResultSetColumnDecoder((column, resultSet) => resultSet.getString(column))
  implicit val intDecoder: ResultSetColumnDecoder[Int]       =
    ResultSetColumnDecoder((column, resultSet) => resultSet.getInt(column))
  implicit val longDecoder: ResultSetColumnDecoder[Long]     =
    ResultSetColumnDecoder((column, resultSet) => resultSet.getLong(column))

  implicit val doubleDecoder: ResultSetColumnDecoder[Double]                                                    =
    ResultSetColumnDecoder((column, resultSet) => resultSet.getDouble(column))
  implicit val floatDecoder: ResultSetColumnDecoder[Float]                                                      =
    ResultSetColumnDecoder((column, resultSet) => resultSet.getFloat(column))
  implicit val shortDecoder: ResultSetColumnDecoder[Short]                                                      =
    ResultSetColumnDecoder((column, resultSet) => resultSet.getShort(column))
  implicit val byteDecoder: ResultSetColumnDecoder[Byte]                                                        =
    ResultSetColumnDecoder((column, resultSet) => resultSet.getByte(column))
  implicit val booleanDecoder: ResultSetColumnDecoder[Boolean]                                                  =
    ResultSetColumnDecoder((column, resultSet) => resultSet.getBoolean(column))
  implicit val bigDecimalDecoder: ResultSetColumnDecoder[BigDecimal]                                            =
    ResultSetColumnDecoder((column, resultSet) => resultSet.getBigDecimal(column))
  implicit val blobDecoder: ResultSetColumnDecoder[Blob]                                                        =
    ResultSetColumnDecoder((column, resultSet) => resultSet.getBlob(column))
  implicit def optionDecoder[A](implicit decoder: ResultSetColumnDecoder[A]): ResultSetColumnDecoder[Option[A]] =
    ResultSetColumnDecoder((column, resultSet) =>
      if (resultSet.getObject(column) == null) None
      else Some(decoder.decode(column, resultSet))
    )
}
