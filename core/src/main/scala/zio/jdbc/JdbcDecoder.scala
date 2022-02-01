package zio.jdbc

import java.sql.ResultSet

import java.sql.Blob

trait JdbcDecoder[+A] {
  def decode(rs: ResultSet): A

  final def map[B](f: A => B): JdbcDecoder[B] = rs => f(decode(rs))
}
object JdbcDecoder    {
  def apply[A](implicit decoder: JdbcDecoder[A]): JdbcDecoder[A] = decoder

  def apply[A](f: ResultSet => A, expected: String = "value"): JdbcDecoder[A] = new JdbcDecoder[A] {
    def decode(rs: ResultSet): A =
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
    JdbcDecoder(rs => (a.decode(1, rs), b.decode(2, rs)))

  implicit def tuple3Decoder[A, B, C](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C]
  ): JdbcDecoder[(A, B, C)] =
    JdbcDecoder(rs => (a.decode(1, rs), b.decode(2, rs), c.decode(3, rs)))

  implicit def tuple4Decoder[A, B, C, D](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D]
  ): JdbcDecoder[(A, B, C, D)] =
    JdbcDecoder(rs => (a.decode(1, rs), b.decode(2, rs), c.decode(3, rs), d.decode(4, rs)))

  implicit def tuple5Decoder[A, B, C, D, E](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E]
  ): JdbcDecoder[(A, B, C, D, E)] =
    JdbcDecoder(rs => (a.decode(1, rs), b.decode(2, rs), c.decode(3, rs), d.decode(4, rs), e.decode(5, rs)))

  implicit def tuple6Decoder[A, B, C, D, E, F](implicit
    a: JdbcColumnDecoder[A],
    b: JdbcColumnDecoder[B],
    c: JdbcColumnDecoder[C],
    d: JdbcColumnDecoder[D],
    e: JdbcColumnDecoder[E],
    f: JdbcColumnDecoder[F]
  ): JdbcDecoder[(A, B, C, D, E, F)] =
    JdbcDecoder(rs =>
      (a.decode(1, rs), b.decode(2, rs), c.decode(3, rs), d.decode(4, rs), e.decode(5, rs), f.decode(6, rs))
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs),
        l.decode(12, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs),
        l.decode(12, rs),
        m.decode(13, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs),
        l.decode(12, rs),
        m.decode(13, rs),
        n.decode(14, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs),
        l.decode(12, rs),
        m.decode(13, rs),
        n.decode(14, rs),
        o.decode(15, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs),
        l.decode(12, rs),
        m.decode(13, rs),
        n.decode(14, rs),
        o.decode(15, rs),
        p.decode(16, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs),
        l.decode(12, rs),
        m.decode(13, rs),
        n.decode(14, rs),
        o.decode(15, rs),
        p.decode(16, rs),
        q.decode(17, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs),
        l.decode(12, rs),
        m.decode(13, rs),
        n.decode(14, rs),
        o.decode(15, rs),
        p.decode(16, rs),
        q.decode(17, rs),
        r.decode(18, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs),
        l.decode(12, rs),
        m.decode(13, rs),
        n.decode(14, rs),
        o.decode(15, rs),
        p.decode(16, rs),
        q.decode(17, rs),
        r.decode(18, rs),
        s.decode(19, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs),
        l.decode(12, rs),
        m.decode(13, rs),
        n.decode(14, rs),
        o.decode(15, rs),
        p.decode(16, rs),
        q.decode(17, rs),
        r.decode(18, rs),
        s.decode(19, rs),
        t.decode(20, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs),
        l.decode(12, rs),
        m.decode(13, rs),
        n.decode(14, rs),
        o.decode(15, rs),
        p.decode(16, rs),
        q.decode(17, rs),
        r.decode(18, rs),
        s.decode(19, rs),
        t.decode(20, rs),
        u.decode(21, rs)
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
        a.decode(1, rs),
        b.decode(2, rs),
        c.decode(3, rs),
        d.decode(4, rs),
        e.decode(5, rs),
        f.decode(6, rs),
        g.decode(7, rs),
        h.decode(8, rs),
        i.decode(9, rs),
        j.decode(10, rs),
        k.decode(11, rs),
        l.decode(12, rs),
        m.decode(13, rs),
        n.decode(14, rs),
        o.decode(15, rs),
        p.decode(16, rs),
        q.decode(17, rs),
        r.decode(18, rs),
        s.decode(19, rs),
        t.decode(20, rs),
        u.decode(21, rs),
        v.decode(22, rs)
      )
    )
}
