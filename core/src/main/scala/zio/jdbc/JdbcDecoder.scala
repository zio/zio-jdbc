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
import java.time.format.DateTimeFormatter
import scala.collection.immutable.ListMap

/**
 * A type class that describes the ability to decode  a value of type `A` from
 * a `ResultSet`.
 */
trait JdbcDecoder[+A] {
  def unsafeDecode(rs: ResultSet): A

  final def decode(rs: ResultSet): Either[Throwable, A] =
    try Right(unsafeDecode(rs))
    catch { case e: JdbcDecoderError => Left(e) }

  final def map[B](f: A => B): JdbcDecoder[B] = rs => f(unsafeDecode(rs))
}

object JdbcDecoder extends JdbcDecoderLowPriorityImplicits {
  def apply[A]()(implicit decoder: JdbcDecoder[A]): JdbcDecoder[A] = decoder

  def apply[A](f: ResultSet => A, expected: String = "value"): JdbcDecoder[A] = new JdbcDecoder[A] {
    def unsafeDecode(rs: ResultSet): A =
      try f(rs)
      catch {
        case t: Throwable if !t.isInstanceOf[VirtualMachineError] =>
          throw JdbcDecoderError(s"Error decoding $expected from ResultSet", t, rs.getMetaData, rs.getRow)
      }
  }

  implicit val intDecoder: JdbcDecoder[Int]                         = JdbcDecoder(_.getInt(1))
  implicit val longDecoder: JdbcDecoder[Long]                       = JdbcDecoder(_.getLong(1))
  implicit val doubleDecoder: JdbcDecoder[Double]                   = JdbcDecoder(_.getDouble(1))
  implicit val stringDecoder: JdbcDecoder[String]                   = JdbcDecoder(_.getString(1))
  implicit val booleanDecoder: JdbcDecoder[Boolean]                 = JdbcDecoder(_.getBoolean(1))
  implicit val bigDecimalDecoder: JdbcDecoder[java.math.BigDecimal] = JdbcDecoder(_.getBigDecimal(1))
  implicit val shortDecoder: JdbcDecoder[Short]                     = JdbcDecoder(_.getShort(1))
  implicit val floatDecoder: JdbcDecoder[Float]                     = JdbcDecoder(_.getFloat(1))
  implicit val byteDecoder: JdbcDecoder[Byte]                       = JdbcDecoder(_.getByte(1))
  implicit val byteArrayDecoder: JdbcDecoder[Array[Byte]]           = JdbcDecoder(_.getBytes(1))
  implicit val blobDecoder: JdbcDecoder[Blob]                       = JdbcDecoder(_.getBlob(1))
  implicit val dateDecoder: JdbcDecoder[java.sql.Date]              = JdbcDecoder(_.getDate(1))
  implicit val timeDecoder: JdbcDecoder[java.sql.Time]              = JdbcDecoder(_.getTime(1))
  implicit val timestampDecoder: JdbcDecoder[java.sql.Timestamp]    = JdbcDecoder(_.getTimestamp(1))

  implicit def tuple2Decoder[A, B](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B]
  ): JdbcDecoder[(A, B)] =
    JdbcDecoder(rs => (a.unsafeDecode(1, rs), b.unsafeDecode(2, rs)))

  implicit def tuple3Decoder[A, B, C](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C]
  ): JdbcDecoder[(A, B, C)] =
    JdbcDecoder(rs => (a.unsafeDecode(1, rs), b.unsafeDecode(2, rs), c.unsafeDecode(3, rs)))

  implicit def tuple4Decoder[A, B, C, D](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D]
  ): JdbcDecoder[(A, B, C, D)] =
    JdbcDecoder(rs => (a.unsafeDecode(1, rs), b.unsafeDecode(2, rs), c.unsafeDecode(3, rs), d.unsafeDecode(4, rs)))

  implicit def tuple5Decoder[A, B, C, D, E](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E]
  ): JdbcDecoder[(A, B, C, D, E)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs)
      )
    )

  implicit def tuple6Decoder[A, B, C, D, E, F](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F]
  ): JdbcDecoder[(A, B, C, D, E, F)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs)
      )
    )

  implicit def tuple7Decoder[A, B, C, D, E, F, G](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G]
  ): JdbcDecoder[(A, B, C, D, E, F, G)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs)
      )
    )

  implicit def tuple8Decoder[A, B, C, D, E, F, G, H](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs)
      )
    )

  implicit def tuple9Decoder[A, B, C, D, E, F, G, H, I](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs)
      )
    )

  implicit def tuple10Decoder[A, B, C, D, E, F, G, H, I, J](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs)
      )
    )

  implicit def tuple11Decoder[A, B, C, D, E, F, G, H, I, J, K](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs)
      )
    )

  implicit def tuple12Decoder[A, B, C, D, E, F, G, H, I, J, K, L](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K],
    l: JdbcColumnDecoder[L]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs),
        l.unsafeDecode(12, rs)
      )
    )

  implicit def tuple13Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K],
    l: JdbcColumnDecoder[L],
    m: JdbcColumnDecoder[M]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs),
        l.unsafeDecode(12, rs),
        m.unsafeDecode(13, rs)
      )
    )

  implicit def tuple14Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K],
    l: JdbcColumnDecoder[L],
    m: JdbcColumnDecoder[M],
    n: JdbcColumnDecoder[N]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs),
        l.unsafeDecode(12, rs),
        m.unsafeDecode(13, rs),
        n.unsafeDecode(14, rs)
      )
    )

  implicit def tuple15Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K],
    l: JdbcColumnDecoder[L],
    m: JdbcColumnDecoder[M],
    n: JdbcColumnDecoder[N],
    o: JdbcColumnDecoder[O]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs),
        l.unsafeDecode(12, rs),
        m.unsafeDecode(13, rs),
        n.unsafeDecode(14, rs),
        o.unsafeDecode(15, rs)
      )
    )

  implicit def tuple16Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K],
    l: JdbcColumnDecoder[L],
    m: JdbcColumnDecoder[M],
    n: JdbcColumnDecoder[N],
    o: JdbcColumnDecoder[O],
    p: JdbcColumnDecoder[P]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs),
        l.unsafeDecode(12, rs),
        m.unsafeDecode(13, rs),
        n.unsafeDecode(14, rs),
        o.unsafeDecode(15, rs),
        p.unsafeDecode(16, rs)
      )
    )

  implicit def tuple17Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K],
    l: JdbcColumnDecoder[L],
    m: JdbcColumnDecoder[M],
    n: JdbcColumnDecoder[N],
    o: JdbcColumnDecoder[O],
    p: JdbcColumnDecoder[P],
    q: JdbcColumnDecoder[Q]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs),
        l.unsafeDecode(12, rs),
        m.unsafeDecode(13, rs),
        n.unsafeDecode(14, rs),
        o.unsafeDecode(15, rs),
        p.unsafeDecode(16, rs),
        q.unsafeDecode(17, rs)
      )
    )

  implicit def tuple18Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K],
    l: JdbcColumnDecoder[L],
    m: JdbcColumnDecoder[M],
    n: JdbcColumnDecoder[N],
    o: JdbcColumnDecoder[O],
    p: JdbcColumnDecoder[P],
    q: JdbcColumnDecoder[Q],
    r: JdbcColumnDecoder[R]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs),
        l.unsafeDecode(12, rs),
        m.unsafeDecode(13, rs),
        n.unsafeDecode(14, rs),
        o.unsafeDecode(15, rs),
        p.unsafeDecode(16, rs),
        q.unsafeDecode(17, rs),
        r.unsafeDecode(18, rs)
      )
    )

  implicit def tuple19Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K],
    l: JdbcColumnDecoder[L],
    m: JdbcColumnDecoder[M],
    n: JdbcColumnDecoder[N],
    o: JdbcColumnDecoder[O],
    p: JdbcColumnDecoder[P],
    q: JdbcColumnDecoder[Q],
    r: JdbcColumnDecoder[R],
    s: JdbcColumnDecoder[S]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs),
        l.unsafeDecode(12, rs),
        m.unsafeDecode(13, rs),
        n.unsafeDecode(14, rs),
        o.unsafeDecode(15, rs),
        p.unsafeDecode(16, rs),
        q.unsafeDecode(17, rs),
        r.unsafeDecode(18, rs),
        s.unsafeDecode(19, rs)
      )
    )

  implicit def tuple20Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K],
    l: JdbcColumnDecoder[L],
    m: JdbcColumnDecoder[M],
    n: JdbcColumnDecoder[N],
    o: JdbcColumnDecoder[O],
    p: JdbcColumnDecoder[P],
    q: JdbcColumnDecoder[Q],
    r: JdbcColumnDecoder[R],
    s: JdbcColumnDecoder[S],
    t: JdbcColumnDecoder[T]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs),
        l.unsafeDecode(12, rs),
        m.unsafeDecode(13, rs),
        n.unsafeDecode(14, rs),
        o.unsafeDecode(15, rs),
        p.unsafeDecode(16, rs),
        q.unsafeDecode(17, rs),
        r.unsafeDecode(18, rs),
        s.unsafeDecode(19, rs),
        t.unsafeDecode(20, rs)
      )
    )

  implicit def tuple21Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K],
    l: JdbcColumnDecoder[L],
    m: JdbcColumnDecoder[M],
    n: JdbcColumnDecoder[N],
    o: JdbcColumnDecoder[O],
    p: JdbcColumnDecoder[P],
    q: JdbcColumnDecoder[Q],
    r: JdbcColumnDecoder[R],
    s: JdbcColumnDecoder[S],
    t: JdbcColumnDecoder[T],
    u: JdbcColumnDecoder[U]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs),
        l.unsafeDecode(12, rs),
        m.unsafeDecode(13, rs),
        n.unsafeDecode(14, rs),
        o.unsafeDecode(15, rs),
        p.unsafeDecode(16, rs),
        q.unsafeDecode(17, rs),
        r.unsafeDecode(18, rs),
        s.unsafeDecode(19, rs),
        t.unsafeDecode(20, rs),
        u.unsafeDecode(21, rs)
      )
    )

  implicit def tuple22Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F],
    g: JdbcColumnDecoder[G],
    h: JdbcColumnDecoder[H],
    i: JdbcColumnDecoder[I],
    j: JdbcColumnDecoder[J],
    k: JdbcColumnDecoder[K],
    l: JdbcColumnDecoder[L],
    m: JdbcColumnDecoder[M],
    n: JdbcColumnDecoder[N],
    o: JdbcColumnDecoder[O],
    p: JdbcColumnDecoder[P],
    q: JdbcColumnDecoder[Q],
    r: JdbcColumnDecoder[R],
    s: JdbcColumnDecoder[S],
    t: JdbcColumnDecoder[T],
    u: JdbcColumnDecoder[U],
    v: JdbcColumnDecoder[V]
  ): JdbcDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    JdbcDecoder(rs =>
      (
        a.unsafeDecode(1, rs),
        b.unsafeDecode(2, rs),
        c.unsafeDecode(3, rs),
        d.unsafeDecode(4, rs),
        e.unsafeDecode(5, rs),
        f.unsafeDecode(6, rs),
        g.unsafeDecode(7, rs),
        h.unsafeDecode(8, rs),
        i.unsafeDecode(9, rs),
        j.unsafeDecode(10, rs),
        k.unsafeDecode(11, rs),
        l.unsafeDecode(12, rs),
        m.unsafeDecode(13, rs),
        n.unsafeDecode(14, rs),
        o.unsafeDecode(15, rs),
        p.unsafeDecode(16, rs),
        q.unsafeDecode(17, rs),
        r.unsafeDecode(18, rs),
        s.unsafeDecode(19, rs),
        t.unsafeDecode(20, rs),
        u.unsafeDecode(21, rs),
        v.unsafeDecode(22, rs)
      )
    )
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
    (rs: ResultSet) => {
      val dynamicDecoder = createDynamicDecoder(schema, rs.getMetaData())
      val dynamicValue   = dynamicDecoder(rs)

      dynamicValue.toTypedValue(schema) match {
        case Left(error) => throw JdbcDecoderError(error, null, rs.getMetaData(), rs.getRow())

        case Right(value) => value
      }
    }
}
