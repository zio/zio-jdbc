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

import java.sql._

/**
 * A type class that describes the ability to decode an `A` from a particular
 * column in a result set.
 */
trait JdbcColumnDecoder[+A] { self =>
  def unsafeDecode(column: Int, resultSet: ResultSet): A

  final def decode(column: Int, resultSet: ResultSet): Either[Throwable, A] =
    try Right(unsafeDecode(column, resultSet))
    catch {
      case e: JdbcDecoderError => Left(e)
      case e: SQLException     => Left(e)
    }

  final def map[B](f: A => B): JdbcColumnDecoder[B] = (column, resultSet) => f(self.unsafeDecode(column, resultSet))
}
object JdbcColumnDecoder    {
  def apply[A](f: (Int, ResultSet) => A): JdbcColumnDecoder[A] =
    new JdbcColumnDecoder[A] {
      def unsafeDecode(column: Int, resultSet: ResultSet): A = f(column, resultSet)
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
      else Some(decoder.unsafeDecode(column, resultSet))
    )
}
