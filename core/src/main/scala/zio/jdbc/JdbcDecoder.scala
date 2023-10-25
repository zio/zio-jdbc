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

import zio._

import java.io._
import java.sql.{ Array => _, _ }
import scala.collection.immutable.ListMap

/**
 * A type class that describes the ability to decode  a value of type `A` from
 * a `ResultSet`.
 */
trait JdbcDecoder[+A] { self =>
  def unsafeDecode(columIndex: Int, rs: ResultSet): (Int, A)

  final def decode(columnIndex: Int, rs: ResultSet): Either[Throwable, (Int, A)] =
    try Right(unsafeDecode(columnIndex, rs))
    catch { case e: JdbcDecoderError => Left(e) }

  final def map[B](f: A => B): JdbcDecoder[B] =
    new JdbcDecoder[B] {
      override def unsafeDecode(
        inputColumnIndex: Int,
        inputResultSet: ResultSet
      ): (Int, B) = {
        val (columnIndex, a) = self.unsafeDecode(inputColumnIndex, inputResultSet)
        (columnIndex, f(a))
      }
    }

  final def flatMap[B](f: A => JdbcDecoder[B]): JdbcDecoder[B] =
    new JdbcDecoder[B] {
      override def unsafeDecode(
        inputColumnIndex: Int,
        inputResultSet: ResultSet
      ): (Int, B) = {
        val (columnIndex, a) = self.unsafeDecode(inputColumnIndex, inputResultSet)
        f(a).unsafeDecode(columnIndex + 1, inputResultSet)
      }
    }

  final def zip[A1 >: A, B, C](
    that: => JdbcDecoder[B]
  )(implicit Z: Zippable.Out[A1, B, C]): JdbcDecoder[C] =
    self.flatMap(a => that.map(b => Z.zip(a, b)))

}

object JdbcDecoder extends JdbcDecoderLowPriorityImplicits {

  final case class RowState(rs: ResultSet, columnIndex: Int)

  def readPrimitive[A](n: Int)(implicit A: JdbcDecoder[A]): ResultSet => A = { (rs: ResultSet) =>
    A.unsafeDecode(n, rs)._2
  }

  def apply[A]()(implicit decoder: JdbcDecoder[A]): JdbcDecoder[A] = decoder

  def apply[A](f: ResultSet => (Int => A), expected: String = "value"): JdbcDecoder[A] =
    new JdbcDecoder[A] {
      override def unsafeDecode(
        inputColumnIndex: Int,
        inputResultSet: ResultSet
      ): (Int, A) =
        try {
          val newValue = f(inputResultSet)(inputColumnIndex)
          (inputColumnIndex, newValue)
        } catch {
          case t: Throwable if !t.isInstanceOf[VirtualMachineError] =>
            throw JdbcDecoderError(
              s"Error decoding $expected from ResultSet",
              t,
              inputResultSet.getMetaData,
              inputResultSet.getRow
            )
        }
    }

  implicit val intDecoder: JdbcDecoder[Int]                         = JdbcDecoder(_.getInt)
  implicit val longDecoder: JdbcDecoder[Long]                       = JdbcDecoder(_.getLong)
  implicit val doubleDecoder: JdbcDecoder[Double]                   = JdbcDecoder(_.getDouble)
  implicit val stringDecoder: JdbcDecoder[String]                   = JdbcDecoder(_.getString)
  implicit val booleanDecoder: JdbcDecoder[Boolean]                 = JdbcDecoder(_.getBoolean)
  implicit val bigDecimalDecoder: JdbcDecoder[java.math.BigDecimal] = JdbcDecoder(_.getBigDecimal)
  implicit val shortDecoder: JdbcDecoder[Short]                     = JdbcDecoder(_.getShort)
  implicit val floatDecoder: JdbcDecoder[Float]                     = JdbcDecoder(_.getFloat)
  implicit val byteDecoder: JdbcDecoder[Byte]                       = JdbcDecoder(_.getByte)
  implicit val byteArrayDecoder: JdbcDecoder[Array[Byte]]           = JdbcDecoder(_.getBytes)
  implicit val blobDecoder: JdbcDecoder[Blob]                       = JdbcDecoder(_.getBlob)
  implicit val dateDecoder: JdbcDecoder[java.sql.Date]              = JdbcDecoder(_.getDate)
  implicit val timeDecoder: JdbcDecoder[java.sql.Time]              = JdbcDecoder(_.getTime)
  implicit val uuidDecoder: JdbcDecoder[java.util.UUID] =
    // See: https://stackoverflow.com/a/56267754/2431728
    JdbcDecoder(rs => i => rs.getObject(i, classOf[java.util.UUID]), "UUID")

  implicit def optionDecoder[A](implicit decoder: JdbcDecoder[A]): JdbcDecoder[Option[A]] =
    JdbcDecoder(rs =>
      int =>
        decoder.decode(int, rs) match {
          case Left(_)      => None
          case Right(value) => Option(value._2)
        }
    )

  implicit def tuple2Decoder[A, B](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B]
  ): JdbcDecoder[(A, B)] =
    a.zip(b)

  implicit def tuple3Decoder[A, B, C](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C]
  ): JdbcDecoder[(A, B, C)] =
    a.zip(b).zip(c)

  // Use Zipper typeclass to form the rest
  implicit def tuple4Decoder[A, B, C, D](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D]
  ): JdbcDecoder[(A, B, C, D)] =
    a.zip(b).zip(c).zip(d)

  implicit def tuple5Decoder[A, B, C, D, E](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E]
  ): JdbcDecoder[(A, B, C, D, E)] =
    a.zip(b).zip(c).zip(d).zip(e)

  implicit def tuple6Decoder[A, B, C, D, E, F](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F]
  ): JdbcDecoder[(A, B, C, D, E, F)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f)

  implicit def tuple7Decoder[A, B, C, D, E, F, G](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G]
  ): JdbcDecoder[(A, B, C, D, E, F, G)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f).zip(g)

  implicit def tuple8Decoder[A, B, C, D, E, F, G, H](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f).zip(g).zip(h)

  implicit def tuple9Decoder[A, B, C, D, E, F, G, H, I](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f).zip(g).zip(h).zip(i)

  implicit def tuple10Decoder[A, B, C, D, E, F, G, H, I, J](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f).zip(g).zip(h).zip(i).zip(j)

  implicit def tuple11Decoder[A, B, C, D, E, F, G, H, I, J, K](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f).zip(g).zip(h).zip(i).zip(j).zip(k)

  implicit def tuple12Decoder[A, B, C, D, E, F, G, H, I, J, K, L](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K],
    l: JdbcDecoder[L]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f).zip(g).zip(h).zip(i).zip(j).zip(k).zip(l)

  implicit def tuple13Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K],
    l: JdbcDecoder[L],
    m: JdbcDecoder[M]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f).zip(g).zip(h).zip(i).zip(j).zip(k).zip(l).zip(m)

  implicit def tuple14Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K],
    l: JdbcDecoder[L],
    m: JdbcDecoder[M],
    n: JdbcDecoder[N]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f).zip(g).zip(h).zip(i).zip(j).zip(k).zip(l).zip(m).zip(n)

  implicit def tuple15Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K],
    l: JdbcDecoder[L],
    m: JdbcDecoder[M],
    n: JdbcDecoder[N],
    o: JdbcDecoder[O]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f).zip(g).zip(h).zip(i).zip(j).zip(k).zip(l).zip(m).zip(n).zip(o)

  implicit def tuple16Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K],
    l: JdbcDecoder[L],
    m: JdbcDecoder[M],
    n: JdbcDecoder[N],
    o: JdbcDecoder[O],
    p: JdbcDecoder[P]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f).zip(g).zip(h).zip(i).zip(j).zip(k).zip(l).zip(m).zip(n).zip(o).zip(p)

  implicit def tuple17Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K],
    l: JdbcDecoder[L],
    m: JdbcDecoder[M],
    n: JdbcDecoder[N],
    o: JdbcDecoder[O],
    p: JdbcDecoder[P],
    q: JdbcDecoder[Q]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    a.zip(b).zip(c).zip(d).zip(e).zip(f).zip(g).zip(h).zip(i).zip(j).zip(k).zip(l).zip(m).zip(n).zip(o).zip(p).zip(q)

  implicit def tuple18Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K],
    l: JdbcDecoder[L],
    m: JdbcDecoder[M],
    n: JdbcDecoder[N],
    o: JdbcDecoder[O],
    p: JdbcDecoder[P],
    q: JdbcDecoder[Q],
    r: JdbcDecoder[R]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    a.zip(b)
      .zip(c)
      .zip(d)
      .zip(e)
      .zip(f)
      .zip(g)
      .zip(h)
      .zip(i)
      .zip(j)
      .zip(k)
      .zip(l)
      .zip(m)
      .zip(n)
      .zip(o)
      .zip(p)
      .zip(q)
      .zip(r)

  implicit def tuple19Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K],
    l: JdbcDecoder[L],
    m: JdbcDecoder[M],
    n: JdbcDecoder[N],
    o: JdbcDecoder[O],
    p: JdbcDecoder[P],
    q: JdbcDecoder[Q],
    r: JdbcDecoder[R],
    s: JdbcDecoder[S]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    a.zip(b)
      .zip(c)
      .zip(d)
      .zip(e)
      .zip(f)
      .zip(g)
      .zip(h)
      .zip(i)
      .zip(j)
      .zip(k)
      .zip(l)
      .zip(m)
      .zip(n)
      .zip(o)
      .zip(p)
      .zip(q)
      .zip(r)
      .zip(s)

  implicit def tuple20Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K],
    l: JdbcDecoder[L],
    m: JdbcDecoder[M],
    n: JdbcDecoder[N],
    o: JdbcDecoder[O],
    p: JdbcDecoder[P],
    q: JdbcDecoder[Q],
    r: JdbcDecoder[R],
    s: JdbcDecoder[S],
    t: JdbcDecoder[T]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    a.zip(b)
      .zip(c)
      .zip(d)
      .zip(e)
      .zip(f)
      .zip(g)
      .zip(h)
      .zip(i)
      .zip(j)
      .zip(k)
      .zip(l)
      .zip(m)
      .zip(n)
      .zip(o)
      .zip(p)
      .zip(q)
      .zip(r)
      .zip(s)
      .zip(t)

  implicit def tuple21Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K],
    l: JdbcDecoder[L],
    m: JdbcDecoder[M],
    n: JdbcDecoder[N],
    o: JdbcDecoder[O],
    p: JdbcDecoder[P],
    q: JdbcDecoder[Q],
    r: JdbcDecoder[R],
    s: JdbcDecoder[S],
    t: JdbcDecoder[T],
    u: JdbcDecoder[U]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    a.zip(b)
      .zip(c)
      .zip(d)
      .zip(e)
      .zip(f)
      .zip(g)
      .zip(h)
      .zip(i)
      .zip(j)
      .zip(k)
      .zip(l)
      .zip(m)
      .zip(n)
      .zip(o)
      .zip(p)
      .zip(q)
      .zip(r)
      .zip(s)
      .zip(t)
      .zip(u)

  implicit def tuple22Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit
    a: JdbcDecoder[A],
    b: JdbcDecoder[B],
    c: JdbcDecoder[C],
    d: JdbcDecoder[D],
    e: JdbcDecoder[E],
    f: JdbcDecoder[F],
    g: JdbcDecoder[G],
    h: JdbcDecoder[H],
    i: JdbcDecoder[I],
    j: JdbcDecoder[J],
    k: JdbcDecoder[K],
    l: JdbcDecoder[L],
    m: JdbcDecoder[M],
    n: JdbcDecoder[N],
    o: JdbcDecoder[O],
    p: JdbcDecoder[P],
    q: JdbcDecoder[Q],
    r: JdbcDecoder[R],
    s: JdbcDecoder[S],
    t: JdbcDecoder[T],
    u: JdbcDecoder[U],
    v: JdbcDecoder[V]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    a.zip(b)
      .zip(c)
      .zip(d)
      .zip(e)
      .zip(f)
      .zip(g)
      .zip(h)
      .zip(i)
      .zip(j)
      .zip(k)
      .zip(l)
      .zip(m)
      .zip(n)
      .zip(o)
      .zip(p)
      .zip(q)
      .zip(r)
      .zip(s)
      .zip(t)
      .zip(u)
      .zip(v)

}

trait JdbcDecoderLowPriorityImplicits {
  import zio.schema._
  import java.sql.{ Types => SqlTypes }

  private def getBinary(binary: InputStream): Chunk[Byte] = {
    val baos = new ByteArrayOutputStream()

    val buffer = Array.ofDim[Byte](1024)

    var read = binary.read(buffer)

    while (read >= 0) {
      baos.write(buffer, 0, read)

      read = binary.read(buffer)
    }

    Chunk.fromArray(baos.toByteArray())
  }

  private[jdbc] def createRemapTable(schema: Schema[_]): String => String =
    schema match {
      case record: Schema.Record[_] =>
        val recordNames = record.fields.map(field => (field.name.toLowerCase, field.name)).toMap

        columnName => recordNames.getOrElse(columnName.toLowerCase, columnName)

      case _ => identity[String](_)
    }

  private[jdbc] def createDynamicDecoder(schema: Schema[_], meta: ResultSetMetaData): ResultSet => DynamicValue =
    resultSet => {
      val remapName   = createRemapTable(schema)
      var columnIndex = 1
      var listMap     = ListMap.empty[String, DynamicValue]

      while (columnIndex <= meta.getColumnCount()) {
        val name = remapName(meta.getColumnName(columnIndex))

        val value: DynamicValue =
          meta.getColumnType(columnIndex) match {
            case SqlTypes.ARRAY =>
              val array = resultSet.getArray(columnIndex)

              createDynamicDecoder(schema, array.getResultSet().getMetaData())(array.getResultSet())

            case SqlTypes.BIGINT =>
              val bigInt = resultSet.getBigDecimal(columnIndex).toBigInteger()

              DynamicValue.Primitive(bigInt, StandardType.BigIntegerType)

            case SqlTypes.BINARY =>
              val chunk = getBinary(resultSet.getBinaryStream(columnIndex))

              DynamicValue.Primitive(chunk, StandardType.BinaryType)

            case SqlTypes.BIT =>
              val bit = resultSet.getInt(columnIndex) == 1

              DynamicValue.Primitive(bit, StandardType.BoolType)

            case SqlTypes.BLOB =>
              val blob = resultSet.getBlob(columnIndex)

              DynamicValue.Primitive(Chunk.fromArray(blob.getBytes(0, blob.length().toInt)), StandardType.BinaryType)

            case SqlTypes.BOOLEAN =>
              val bool = resultSet.getBoolean(columnIndex)

              DynamicValue.Primitive(bool, StandardType.BoolType)

            case SqlTypes.CHAR =>
              val char: Char = resultSet.getString(columnIndex)(0)

              DynamicValue.Primitive(char, StandardType.CharType)

            case SqlTypes.CLOB =>
              val clob = resultSet.getClob(columnIndex)

              DynamicValue.Primitive(clob.getSubString(0L, clob.length().toInt), StandardType.StringType)

            case SqlTypes.DATE =>
              val date = resultSet.getDate(columnIndex)

              DynamicValue.Primitive(date.toLocalDate(), StandardType.LocalDateType)

            case SqlTypes.DECIMAL =>
              val bigDecimal = resultSet.getBigDecimal(columnIndex)

              DynamicValue.Primitive(bigDecimal, StandardType.BigDecimalType)

            case SqlTypes.DOUBLE =>
              val double = resultSet.getDouble(columnIndex)

              DynamicValue.Primitive(double, StandardType.DoubleType)

            case SqlTypes.FLOAT =>
              val float = resultSet.getFloat(columnIndex)

              DynamicValue.Primitive(float, StandardType.FloatType)

            case SqlTypes.INTEGER =>
              val int = resultSet.getInt(columnIndex)

              DynamicValue.Primitive(int, StandardType.IntType)

            case SqlTypes.LONGNVARCHAR =>
              val string = resultSet.getString(columnIndex)

              DynamicValue.Primitive(string, StandardType.StringType)

            case SqlTypes.LONGVARBINARY =>
              val chunk = getBinary(resultSet.getBinaryStream(columnIndex))

              DynamicValue.Primitive(chunk, StandardType.BinaryType)

            case SqlTypes.LONGVARCHAR =>
              val string = resultSet.getString(columnIndex)

              DynamicValue.Primitive(string, StandardType.StringType)

            case SqlTypes.NCHAR =>
              val string = resultSet.getNString(columnIndex)

              DynamicValue.Primitive(string, StandardType.StringType)

            case SqlTypes.NCLOB =>
              val clob = resultSet.getNClob(columnIndex)

              DynamicValue.Primitive(clob.getSubString(0L, clob.length().toInt), StandardType.StringType)

            case SqlTypes.NULL =>
              DynamicValue.Primitive((), StandardType.UnitType)

            case SqlTypes.NUMERIC =>
              val bigDecimal = resultSet.getBigDecimal(columnIndex)

              DynamicValue.Primitive(bigDecimal, StandardType.BigDecimalType)

            case SqlTypes.NVARCHAR =>
              val string = resultSet.getString(columnIndex)

              DynamicValue.Primitive(string, StandardType.StringType)

            case SqlTypes.REAL =>
              val bigDecimal = resultSet.getBigDecimal(columnIndex)

              DynamicValue.Primitive(bigDecimal, StandardType.BigDecimalType)

            case SqlTypes.ROWID =>
              val long = resultSet.getLong(columnIndex)

              DynamicValue.Primitive(long, StandardType.LongType)

            case SqlTypes.SMALLINT =>
              val short = resultSet.getShort(columnIndex)

              DynamicValue.Primitive(short, StandardType.ShortType)

            case SqlTypes.SQLXML =>
              val xml = resultSet.getSQLXML(columnIndex)

              DynamicValue.Primitive(xml.getString(), StandardType.StringType)

            case SqlTypes.TIME =>
              val time = resultSet.getTime(columnIndex)

              DynamicValue.Primitive(time.toLocalTime(), StandardType.LocalTimeType)

            case SqlTypes.TIMESTAMP =>
              val timestamp = resultSet.getTimestamp(columnIndex)

              DynamicValue.Primitive(timestamp.toInstant(), StandardType.InstantType)

            case SqlTypes.TIMESTAMP_WITH_TIMEZONE =>
              // TODO: Timezone
              val timestamp = resultSet.getTimestamp(columnIndex)

              DynamicValue.Primitive(timestamp.toInstant(), StandardType.InstantType)

            case SqlTypes.TIME_WITH_TIMEZONE =>
              // TODO: Timezone
              val time = resultSet.getTime(columnIndex)

              DynamicValue.Primitive(time.toLocalTime(), StandardType.LocalTimeType)

            case SqlTypes.TINYINT =>
              val short = resultSet.getShort(columnIndex)

              DynamicValue.Primitive(short, StandardType.ShortType)

            case SqlTypes.VARBINARY =>
              val chunk = getBinary(resultSet.getBinaryStream(columnIndex))

              DynamicValue.Primitive(chunk, StandardType.BinaryType)

            case SqlTypes.VARCHAR =>
              val string = resultSet.getString(columnIndex)

              DynamicValue.Primitive(string, StandardType.StringType)

            case other =>
              throw new SQLException(
                s"Unsupported SQL type ${other} when attempting to decode result set from a ZIO Schema ${schema}"
              )
          }

        listMap = listMap.updated(name, value)

        columnIndex += 1
      }

      DynamicValue.Record(TypeId.Structural, listMap)
    }

  def fromSchema[A](implicit schema: Schema[A]): JdbcDecoder[A] =
    (columnIndex: Int, resultSet: ResultSet) => {
      val dynamicDecoder = createDynamicDecoder(schema, resultSet.getMetaData())
      val dynamicValue   = dynamicDecoder(resultSet)

      dynamicValue.toTypedValue(schema) match {
        case Left(error) => throw JdbcDecoderError(error, null, resultSet.getMetaData(), resultSet.getRow())

        case Right(value) => (columnIndex, value)
      }
    }
}
