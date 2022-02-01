package zio.jdbc

import java.sql.ResultSet

import java.sql.Blob

/**
 * A type class that describes the ability to unsafeDecode a value of type `A` from
 * a `ResultSet`.
 */
trait JdbcDecoder[+A] {
  def unsafeDecode(rs: ResultSet): A

  def decodeSafe(rs: ResultSet): Either[Throwable, A] =
    try Right(unsafeDecode(rs))
    catch { case e: JdbcDecoderError => Left(e) }

  final def map[B](f: A => B): JdbcDecoder[B] = rs => f(unsafeDecode(rs))
}
object JdbcDecoder    {
  def apply[A](implicit decoder: JdbcDecoder[A]): JdbcDecoder[A] = decoder

  def apply[A](f: ResultSet => A, expected: String = "value"): JdbcDecoder[A] = new JdbcDecoder[A] {
    def unsafeDecode(rs: ResultSet): A =
      try f(rs)
      catch {
        case t: Throwable if !t.isInstanceOf[VirtualMachineError] =>
          throw JdbcDecoderError(s"Error decoding $expected from ResultSet", t, rs.getMetaData(), rs.getRow())
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
