package zio.jdbc

import java.sql.ResultSet

import java.sql.Blob

trait ResultSetDecoder[+A] {
  def decode(rs: ResultSet): A
}
object ResultSetDecoder    {
  def apply[A](f: ResultSet => A, expected: String = "value"): ResultSetDecoder[A] = new ResultSetDecoder[A] {
    def decode(rs: ResultSet): A =
      try f(rs)
      catch {
        case t: Throwable if !t.isInstanceOf[VirtualMachineError] =>
          throw ResultSetDecoderError(s"Error decoding $expected from ResultSet", t, rs.getMetaData(), rs.getRow())
      }
  }

  implicit val intDecoder: ResultSetDecoder[Int]               = ResultSetDecoder(_.getInt(0))
  implicit val longDecoder: ResultSetDecoder[Long]             = ResultSetDecoder(_.getLong(0))
  implicit val doubleDecoder: ResultSetDecoder[Double]         = ResultSetDecoder(_.getDouble(0))
  implicit val stringDecoder: ResultSetDecoder[String]         = ResultSetDecoder(_.getString(0))
  implicit val booleanDecoder: ResultSetDecoder[Boolean]       = ResultSetDecoder(_.getBoolean(0))
  implicit val bigDecimalDecoder: ResultSetDecoder[BigDecimal] = ResultSetDecoder(_.getBigDecimal(0))
  implicit val shortDecoder: ResultSetDecoder[Short]           = ResultSetDecoder(_.getShort(0))
  implicit val floatDecoder: ResultSetDecoder[Float]           = ResultSetDecoder(_.getFloat(0))
  implicit val byteDecoder: ResultSetDecoder[Byte]             = ResultSetDecoder(_.getByte(0))
  implicit val byteArrayDecoder: ResultSetDecoder[Array[Byte]] = ResultSetDecoder(_.getBytes(0))
  implicit val blobDecoder: ResultSetDecoder[Blob]             = ResultSetDecoder(_.getBlob(0))

  implicit def tuple2Decoder[A, B](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B]
  ): ResultSetDecoder[(A, B)] =
    ResultSetDecoder(rs => (a.decode(0, rs), b.decode(1, rs)))

  implicit def tuple3Decoder[A, B, C](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C]
  ): ResultSetDecoder[(A, B, C)] =
    ResultSetDecoder(rs => (a.decode(0, rs), b.decode(1, rs), c.decode(2, rs)))

  implicit def tuple4Decoder[A, B, C, D](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D]
  ): ResultSetDecoder[(A, B, C, D)] =
    ResultSetDecoder(rs => (a.decode(0, rs), b.decode(1, rs), c.decode(2, rs), d.decode(3, rs)))

  implicit def tuple5Decoder[A, B, C, D, E](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E]
  ): ResultSetDecoder[(A, B, C, D, E)] =
    ResultSetDecoder(rs => (a.decode(0, rs), b.decode(1, rs), c.decode(2, rs), d.decode(3, rs), e.decode(4, rs)))

  implicit def tuple6Decoder[A, B, C, D, E, F](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F]
  ): ResultSetDecoder[(A, B, C, D, E, F)] =
    ResultSetDecoder(rs =>
      (a.decode(0, rs), b.decode(1, rs), c.decode(2, rs), d.decode(3, rs), e.decode(4, rs), f.decode(5, rs))
    )

  implicit def tuple7Decoder[A, B, C, D, E, F, G](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G]
  ): ResultSetDecoder[(A, B, C, D, E, F, G)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs)
      )
    )

  implicit def tuple8Decoder[A, B, C, D, E, F, G, H](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs)
      )
    )

  implicit def tuple9Decoder[A, B, C, D, E, F, G, H, I](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs)
      )
    )

  implicit def tuple10Decoder[A, B, C, D, E, F, G, H, I, J](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs)
      )
    )

  implicit def tuple11Decoder[A, B, C, D, E, F, G, H, I, J, K](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs)
      )
    )

  implicit def tuple12Decoder[A, B, C, D, E, F, G, H, I, J, K, L](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K],
    l: ResultSetColumnDecoder[L]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs),
        l.decode(11, rs)
      )
    )

  implicit def tuple13Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K],
    l: ResultSetColumnDecoder[L],
    m: ResultSetColumnDecoder[M]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs),
        l.decode(11, rs),
        m.decode(12, rs)
      )
    )

  implicit def tuple14Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K],
    l: ResultSetColumnDecoder[L],
    m: ResultSetColumnDecoder[M],
    n: ResultSetColumnDecoder[N]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs),
        l.decode(11, rs),
        m.decode(12, rs),
        n.decode(13, rs)
      )
    )

  implicit def tuple15Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K],
    l: ResultSetColumnDecoder[L],
    m: ResultSetColumnDecoder[M],
    n: ResultSetColumnDecoder[N],
    o: ResultSetColumnDecoder[O]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs),
        l.decode(11, rs),
        m.decode(12, rs),
        n.decode(13, rs),
        o.decode(14, rs)
      )
    )

  implicit def tuple16Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K],
    l: ResultSetColumnDecoder[L],
    m: ResultSetColumnDecoder[M],
    n: ResultSetColumnDecoder[N],
    o: ResultSetColumnDecoder[O],
    p: ResultSetColumnDecoder[P]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs),
        l.decode(11, rs),
        m.decode(12, rs),
        n.decode(13, rs),
        o.decode(14, rs),
        p.decode(15, rs)
      )
    )

  implicit def tuple17Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K],
    l: ResultSetColumnDecoder[L],
    m: ResultSetColumnDecoder[M],
    n: ResultSetColumnDecoder[N],
    o: ResultSetColumnDecoder[O],
    p: ResultSetColumnDecoder[P],
    q: ResultSetColumnDecoder[Q]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs),
        l.decode(11, rs),
        m.decode(12, rs),
        n.decode(13, rs),
        o.decode(14, rs),
        p.decode(15, rs),
        q.decode(16, rs)
      )
    )

  implicit def tuple18Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K],
    l: ResultSetColumnDecoder[L],
    m: ResultSetColumnDecoder[M],
    n: ResultSetColumnDecoder[N],
    o: ResultSetColumnDecoder[O],
    p: ResultSetColumnDecoder[P],
    q: ResultSetColumnDecoder[Q],
    r: ResultSetColumnDecoder[R]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs),
        l.decode(11, rs),
        m.decode(12, rs),
        n.decode(13, rs),
        o.decode(14, rs),
        p.decode(15, rs),
        q.decode(16, rs),
        r.decode(17, rs)
      )
    )

  implicit def tuple19Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K],
    l: ResultSetColumnDecoder[L],
    m: ResultSetColumnDecoder[M],
    n: ResultSetColumnDecoder[N],
    o: ResultSetColumnDecoder[O],
    p: ResultSetColumnDecoder[P],
    q: ResultSetColumnDecoder[Q],
    r: ResultSetColumnDecoder[R],
    s: ResultSetColumnDecoder[S]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs),
        l.decode(11, rs),
        m.decode(12, rs),
        n.decode(13, rs),
        o.decode(14, rs),
        p.decode(15, rs),
        q.decode(16, rs),
        r.decode(17, rs),
        s.decode(18, rs)
      )
    )

  implicit def tuple20Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K],
    l: ResultSetColumnDecoder[L],
    m: ResultSetColumnDecoder[M],
    n: ResultSetColumnDecoder[N],
    o: ResultSetColumnDecoder[O],
    p: ResultSetColumnDecoder[P],
    q: ResultSetColumnDecoder[Q],
    r: ResultSetColumnDecoder[R],
    s: ResultSetColumnDecoder[S],
    t: ResultSetColumnDecoder[T]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs),
        l.decode(11, rs),
        m.decode(12, rs),
        n.decode(13, rs),
        o.decode(14, rs),
        p.decode(15, rs),
        q.decode(16, rs),
        r.decode(17, rs),
        s.decode(18, rs),
        t.decode(19, rs)
      )
    )

  implicit def tuple21Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K],
    l: ResultSetColumnDecoder[L],
    m: ResultSetColumnDecoder[M],
    n: ResultSetColumnDecoder[N],
    o: ResultSetColumnDecoder[O],
    p: ResultSetColumnDecoder[P],
    q: ResultSetColumnDecoder[Q],
    r: ResultSetColumnDecoder[R],
    s: ResultSetColumnDecoder[S],
    t: ResultSetColumnDecoder[T],
    u: ResultSetColumnDecoder[U]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs),
        l.decode(11, rs),
        m.decode(12, rs),
        n.decode(13, rs),
        o.decode(14, rs),
        p.decode(15, rs),
        q.decode(16, rs),
        r.decode(17, rs),
        s.decode(18, rs),
        t.decode(19, rs),
        u.decode(20, rs)
      )
    )

  implicit def tuple21Decoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit
    a: ResultSetColumnDecoder[A],
    b: ResultSetColumnDecoder[B],
    c: ResultSetColumnDecoder[C],
    d: ResultSetColumnDecoder[D],
    e: ResultSetColumnDecoder[E],
    f: ResultSetColumnDecoder[F],
    g: ResultSetColumnDecoder[G],
    h: ResultSetColumnDecoder[H],
    i: ResultSetColumnDecoder[I],
    j: ResultSetColumnDecoder[J],
    k: ResultSetColumnDecoder[K],
    l: ResultSetColumnDecoder[L],
    m: ResultSetColumnDecoder[M],
    n: ResultSetColumnDecoder[N],
    o: ResultSetColumnDecoder[O],
    p: ResultSetColumnDecoder[P],
    q: ResultSetColumnDecoder[Q],
    r: ResultSetColumnDecoder[R],
    s: ResultSetColumnDecoder[S],
    t: ResultSetColumnDecoder[T],
    u: ResultSetColumnDecoder[U],
    v: ResultSetColumnDecoder[V]
  ): ResultSetDecoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    ResultSetDecoder(rs =>
      (
        a.decode(0, rs),
        b.decode(1, rs),
        c.decode(2, rs),
        d.decode(3, rs),
        e.decode(4, rs),
        f.decode(5, rs),
        g.decode(6, rs),
        h.decode(7, rs),
        i.decode(8, rs),
        j.decode(9, rs),
        k.decode(10, rs),
        l.decode(11, rs),
        m.decode(12, rs),
        n.decode(13, rs),
        o.decode(14, rs),
        p.decode(15, rs),
        q.decode(16, rs),
        r.decode(17, rs),
        s.decode(18, rs),
        t.decode(19, rs),
        u.decode(20, rs),
        v.decode(21, rs)
      )
    )
}
