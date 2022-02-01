package zio.jdbc

import java.sql.PreparedStatement

trait JdbcEncoder[-A] {
  def encode(preparedStatement: PreparedStatement, value: A): Unit

  final def contramap[B](f: B => A): JdbcEncoder[B] = (preparedStatement, value) => encode(preparedStatement, f(value))
}
object JdbcEncoder    {
  implicit val intEncoder: JdbcEncoder[Int]                               = (preparedStatement, value) => preparedStatement.setInt(1, value)
  implicit val longEncoder: JdbcEncoder[Long]                             = (preparedStatement, value) => preparedStatement.setLong(1, value)
  implicit val doubleEncoder: JdbcEncoder[Double]                         = (preparedStatement, value) => preparedStatement.setDouble(1, value)
  implicit val stringEncoder: JdbcEncoder[String]                         = (preparedStatement, value) => preparedStatement.setString(1, value)
  implicit val booleanEncoder: JdbcEncoder[Boolean]                       = (preparedStatement, value) =>
    preparedStatement.setBoolean(1, value)
  implicit val bigDecimalEncoder: JdbcEncoder[java.math.BigDecimal]       = (preparedStatement, value) =>
    preparedStatement.setBigDecimal(1, value)
  implicit val bigDecimalEncoderScala: JdbcEncoder[scala.math.BigDecimal] = (preparedStatement, value) =>
    preparedStatement.setBigDecimal(1, value.bigDecimal)
  implicit val shortEncoder: JdbcEncoder[Short]                           = (preparedStatement, value) => preparedStatement.setShort(1, value)
  implicit val floatEncoder: JdbcEncoder[Float]                           = (preparedStatement, value) => preparedStatement.setFloat(1, value)
  implicit val byteEncoder: JdbcEncoder[Byte]                             = (preparedStatement, value) => preparedStatement.setByte(1, value)
  implicit val byteArrayEncoder: JdbcEncoder[Array[Byte]]                 = (preparedStatement, value) =>
    preparedStatement.setBytes(1, value)
  implicit val blobEncoder: JdbcEncoder[java.sql.Blob]                    = (preparedStatement, value) =>
    preparedStatement.setBlob(1, value)

  implicit def optionEncoder[A](implicit encoder: JdbcColumnEncoder[A]): JdbcEncoder[Option[A]] =
    (preparedStatement, value) =>
      value.fold(preparedStatement.setObject(1, null))(encoder.encode(1, _, preparedStatement))

  implicit def tuple2Encoder[A, B](implicit a: JdbcColumnEncoder[A], b: JdbcColumnEncoder[B]): JdbcEncoder[(A, B)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
    }

  implicit def tuple3Encoder[A, B, C](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C]
  ): JdbcEncoder[(A, B, C)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
    }

  implicit def tuple4Encoder[A, B, C, D](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D]
  ): JdbcEncoder[(A, B, C, D)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
    }

  implicit def tuple5Encoder[A, B, C, D, E](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E]
  ): JdbcEncoder[(A, B, C, D, E)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
    }

  implicit def tuple6Encoder[A, B, C, D, E, F](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F]
  ): JdbcEncoder[(A, B, C, D, E, F)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
    }

  implicit def tuple7Encoder[A, B, C, D, E, F, G](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G]
  ): JdbcEncoder[(A, B, C, D, E, F, G)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
    }

  implicit def tuple8Encoder[A, B, C, D, E, F, G, H](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
    }

  implicit def tuple9Encoder[A, B, C, D, E, F, G, H, I](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
    }

  implicit def tuple10Encoder[A, B, C, D, E, F, G, H, I, J](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
    }

  implicit def tuple11Encoder[A, B, C, D, E, F, G, H, I, J, K](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
    }

  implicit def tuple12Encoder[A, B, C, D, E, F, G, H, I, J, K, L](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K],
    l: JdbcColumnEncoder[L]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
      l.encode(12, value._12, preparedStatement)
    }

  implicit def tuple13Encoder[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K],
    l: JdbcColumnEncoder[L],
    m: JdbcColumnEncoder[M]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
      l.encode(12, value._12, preparedStatement)
      m.encode(13, value._13, preparedStatement)
    }

  implicit def tuple14Encoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K],
    l: JdbcColumnEncoder[L],
    m: JdbcColumnEncoder[M],
    n: JdbcColumnEncoder[N]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
      l.encode(12, value._12, preparedStatement)
      m.encode(13, value._13, preparedStatement)
      n.encode(14, value._14, preparedStatement)
    }

  implicit def tuple15Encoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K],
    l: JdbcColumnEncoder[L],
    m: JdbcColumnEncoder[M],
    n: JdbcColumnEncoder[N],
    o: JdbcColumnEncoder[O]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
      l.encode(12, value._12, preparedStatement)
      m.encode(13, value._13, preparedStatement)
      n.encode(14, value._14, preparedStatement)
      o.encode(15, value._15, preparedStatement)
    }

  implicit def tuple16Encoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K],
    l: JdbcColumnEncoder[L],
    m: JdbcColumnEncoder[M],
    n: JdbcColumnEncoder[N],
    o: JdbcColumnEncoder[O],
    p: JdbcColumnEncoder[P]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
      l.encode(12, value._12, preparedStatement)
      m.encode(13, value._13, preparedStatement)
      n.encode(14, value._14, preparedStatement)
      o.encode(15, value._15, preparedStatement)
      p.encode(16, value._16, preparedStatement)
    }

  implicit def tuple17Encoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K],
    l: JdbcColumnEncoder[L],
    m: JdbcColumnEncoder[M],
    n: JdbcColumnEncoder[N],
    o: JdbcColumnEncoder[O],
    p: JdbcColumnEncoder[P],
    q: JdbcColumnEncoder[Q]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
      l.encode(12, value._12, preparedStatement)
      m.encode(13, value._13, preparedStatement)
      n.encode(14, value._14, preparedStatement)
      o.encode(15, value._15, preparedStatement)
      p.encode(16, value._16, preparedStatement)
      q.encode(17, value._17, preparedStatement)
    }

  implicit def tuple18Encoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K],
    l: JdbcColumnEncoder[L],
    m: JdbcColumnEncoder[M],
    n: JdbcColumnEncoder[N],
    o: JdbcColumnEncoder[O],
    p: JdbcColumnEncoder[P],
    q: JdbcColumnEncoder[Q],
    r: JdbcColumnEncoder[R]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
      l.encode(12, value._12, preparedStatement)
      m.encode(13, value._13, preparedStatement)
      n.encode(14, value._14, preparedStatement)
      o.encode(15, value._15, preparedStatement)
      p.encode(16, value._16, preparedStatement)
      q.encode(17, value._17, preparedStatement)
      r.encode(18, value._18, preparedStatement)
    }

  implicit def tuple19Encoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K],
    l: JdbcColumnEncoder[L],
    m: JdbcColumnEncoder[M],
    n: JdbcColumnEncoder[N],
    o: JdbcColumnEncoder[O],
    p: JdbcColumnEncoder[P],
    q: JdbcColumnEncoder[Q],
    r: JdbcColumnEncoder[R],
    s: JdbcColumnEncoder[S]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
      l.encode(12, value._12, preparedStatement)
      m.encode(13, value._13, preparedStatement)
      n.encode(14, value._14, preparedStatement)
      o.encode(15, value._15, preparedStatement)
      p.encode(16, value._16, preparedStatement)
      q.encode(17, value._17, preparedStatement)
      r.encode(18, value._18, preparedStatement)
      s.encode(19, value._19, preparedStatement)
    }

  implicit def tuple20Encoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K],
    l: JdbcColumnEncoder[L],
    m: JdbcColumnEncoder[M],
    n: JdbcColumnEncoder[N],
    o: JdbcColumnEncoder[O],
    p: JdbcColumnEncoder[P],
    q: JdbcColumnEncoder[Q],
    r: JdbcColumnEncoder[R],
    s: JdbcColumnEncoder[S],
    t: JdbcColumnEncoder[T]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
      l.encode(12, value._12, preparedStatement)
      m.encode(13, value._13, preparedStatement)
      n.encode(14, value._14, preparedStatement)
      o.encode(15, value._15, preparedStatement)
      p.encode(16, value._16, preparedStatement)
      q.encode(17, value._17, preparedStatement)
      r.encode(18, value._18, preparedStatement)
      s.encode(19, value._19, preparedStatement)
      t.encode(20, value._20, preparedStatement)
    }

  implicit def tuple21Encoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K],
    l: JdbcColumnEncoder[L],
    m: JdbcColumnEncoder[M],
    n: JdbcColumnEncoder[N],
    o: JdbcColumnEncoder[O],
    p: JdbcColumnEncoder[P],
    q: JdbcColumnEncoder[Q],
    r: JdbcColumnEncoder[R],
    s: JdbcColumnEncoder[S],
    t: JdbcColumnEncoder[T],
    u: JdbcColumnEncoder[U]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
      l.encode(12, value._12, preparedStatement)
      m.encode(13, value._13, preparedStatement)
      n.encode(14, value._14, preparedStatement)
      o.encode(15, value._15, preparedStatement)
      p.encode(16, value._16, preparedStatement)
      q.encode(17, value._17, preparedStatement)
      r.encode(18, value._18, preparedStatement)
      s.encode(19, value._19, preparedStatement)
      t.encode(20, value._20, preparedStatement)
      u.encode(21, value._21, preparedStatement)
    }

  implicit def tuple22Encoder[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit
    a: JdbcColumnEncoder[A],
    b: JdbcColumnEncoder[B],
    c: JdbcColumnEncoder[C],
    d: JdbcColumnEncoder[D],
    e: JdbcColumnEncoder[E],
    f: JdbcColumnEncoder[F],
    g: JdbcColumnEncoder[G],
    h: JdbcColumnEncoder[H],
    i: JdbcColumnEncoder[I],
    j: JdbcColumnEncoder[J],
    k: JdbcColumnEncoder[K],
    l: JdbcColumnEncoder[L],
    m: JdbcColumnEncoder[M],
    n: JdbcColumnEncoder[N],
    o: JdbcColumnEncoder[O],
    p: JdbcColumnEncoder[P],
    q: JdbcColumnEncoder[Q],
    r: JdbcColumnEncoder[R],
    s: JdbcColumnEncoder[S],
    t: JdbcColumnEncoder[T],
    u: JdbcColumnEncoder[U],
    v: JdbcColumnEncoder[V]
  ): JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    (preparedStatement, value) => {
      a.encode(1, value._1, preparedStatement)
      b.encode(2, value._2, preparedStatement)
      c.encode(3, value._3, preparedStatement)
      d.encode(4, value._4, preparedStatement)
      e.encode(5, value._5, preparedStatement)
      f.encode(6, value._6, preparedStatement)
      g.encode(7, value._7, preparedStatement)
      h.encode(8, value._8, preparedStatement)
      i.encode(9, value._9, preparedStatement)
      j.encode(10, value._10, preparedStatement)
      k.encode(11, value._11, preparedStatement)
      l.encode(12, value._12, preparedStatement)
      m.encode(13, value._13, preparedStatement)
      n.encode(14, value._14, preparedStatement)
      o.encode(15, value._15, preparedStatement)
      p.encode(16, value._16, preparedStatement)
      q.encode(17, value._17, preparedStatement)
      r.encode(18, value._18, preparedStatement)
      s.encode(19, value._19, preparedStatement)
      t.encode(20, value._20, preparedStatement)
      u.encode(21, value._21, preparedStatement)
      v.encode(22, value._22, preparedStatement)
    }
}
