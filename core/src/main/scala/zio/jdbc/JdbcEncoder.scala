package zio.jdbc

trait JdbcEncoder[-A] {
  def encode(value: A): Sql[ZResultSet]

  final def contramap[B](f: B => A): JdbcEncoder[B] = (value) => encode(f(value))
}
object JdbcEncoder    {
  def apply[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[A] = encoder

  implicit val intEncoder: JdbcEncoder[Int] = value => sql"$value"

  implicit val longEncoder: JdbcEncoder[Long]                             = value => sql"$value"
  implicit val doubleEncoder: JdbcEncoder[Double]                         = value => sql"$value"
  implicit val stringEncoder: JdbcEncoder[String]                         = value => sql"$value"
  implicit val booleanEncoder: JdbcEncoder[Boolean]                       = value => sql"$value"
  implicit val bigDecimalEncoder: JdbcEncoder[java.math.BigDecimal]       = value => sql"$value"
  implicit val bigDecimalEncoderScala: JdbcEncoder[scala.math.BigDecimal] = value => sql"$value"
  implicit val shortEncoder: JdbcEncoder[Short]                           = value => sql"$value"
  implicit val floatEncoder: JdbcEncoder[Float]                           = value => sql"$value"
  implicit val byteEncoder: JdbcEncoder[Byte]                             = value => sql"$value"
  implicit val byteArrayEncoder: JdbcEncoder[Array[Byte]]                 = value => sql"$value"
  implicit val blobEncoder: JdbcEncoder[java.sql.Blob]                    = value => sql"$value"

  implicit def optionEncoder[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[Option[A]] =
    value => value.fold(sql"NULL")(encoder.encode(_))

  implicit def tuple2Encoder[A: JdbcEncoder, B: JdbcEncoder]: JdbcEncoder[(A, B)] =
    tuple => JdbcEncoder[A].encode(tuple._1) + Sql.comma + JdbcEncoder[B].encode(tuple._2)

  implicit def tuple3Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder]: JdbcEncoder[(A, B, C)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) + Sql.comma + JdbcEncoder[B].encode(
        tuple._2
      ) + Sql.comma + JdbcEncoder[C].encode(tuple._3)

  implicit def tuple4Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder, D: JdbcEncoder]
    : JdbcEncoder[(A, B, C, D)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) + Sql.comma + JdbcEncoder[B].encode(
        tuple._2
      ) + Sql.comma + JdbcEncoder[C].encode(tuple._3) + Sql.comma + JdbcEncoder[D].encode(tuple._4)

  implicit def tuple5Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder, D: JdbcEncoder, E: JdbcEncoder]
    : JdbcEncoder[(A, B, C, D, E)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) + Sql.comma + JdbcEncoder[B].encode(
        tuple._2
      ) + Sql.comma + JdbcEncoder[C].encode(tuple._3) + Sql.comma + JdbcEncoder[D].encode(
        tuple._4
      ) + Sql.comma + JdbcEncoder[E].encode(tuple._5)

  implicit def tuple6Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) + Sql.comma + JdbcEncoder[B].encode(
        tuple._2
      ) + Sql.comma + JdbcEncoder[C].encode(tuple._3) + Sql.comma + JdbcEncoder[D].encode(
        tuple._4
      ) + Sql.comma + JdbcEncoder[E].encode(tuple._5) + Sql.comma + JdbcEncoder[F].encode(tuple._6)

  implicit def tuple7Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) + Sql.comma + JdbcEncoder[B].encode(
        tuple._2
      ) + Sql.comma + JdbcEncoder[C].encode(tuple._3) + Sql.comma + JdbcEncoder[D].encode(
        tuple._4
      ) + Sql.comma + JdbcEncoder[E].encode(tuple._5) + Sql.comma + JdbcEncoder[F].encode(
        tuple._6
      ) + Sql.comma + JdbcEncoder[G].encode(tuple._7)

  implicit def tuple8Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) + Sql.comma + JdbcEncoder[B].encode(
        tuple._2
      ) + Sql.comma + JdbcEncoder[C].encode(tuple._3) + Sql.comma + JdbcEncoder[D].encode(
        tuple._4
      ) + Sql.comma + JdbcEncoder[E].encode(tuple._5) + Sql.comma + JdbcEncoder[F].encode(
        tuple._6
      ) + Sql.comma + JdbcEncoder[G].encode(tuple._7) + Sql.comma + JdbcEncoder[H].encode(tuple._8)

  implicit def tuple9Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) + Sql.comma + JdbcEncoder[B].encode(
        tuple._2
      ) + Sql.comma + JdbcEncoder[C].encode(tuple._3) + Sql.comma + JdbcEncoder[D].encode(
        tuple._4
      ) + Sql.comma + JdbcEncoder[E].encode(tuple._5) + Sql.comma + JdbcEncoder[F].encode(
        tuple._6
      ) + Sql.comma + JdbcEncoder[G].encode(tuple._7) + Sql.comma + JdbcEncoder[H].encode(
        tuple._8
      ) + Sql.comma + JdbcEncoder[I].encode(tuple._9)

  implicit def tuple10Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) + Sql.comma + JdbcEncoder[B].encode(
        tuple._2
      ) + Sql.comma + JdbcEncoder[C].encode(tuple._3) + Sql.comma + JdbcEncoder[D].encode(
        tuple._4
      ) + Sql.comma + JdbcEncoder[E].encode(tuple._5) + Sql.comma + JdbcEncoder[F].encode(
        tuple._6
      ) + Sql.comma + JdbcEncoder[G].encode(tuple._7) + Sql.comma + JdbcEncoder[H].encode(
        tuple._8
      ) + Sql.comma + JdbcEncoder[I].encode(tuple._9) + Sql.comma + JdbcEncoder[J].encode(tuple._10)

  implicit def tuple11Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) + Sql.comma + JdbcEncoder[B].encode(
        tuple._2
      ) + Sql.comma + JdbcEncoder[C].encode(tuple._3) + Sql.comma + JdbcEncoder[D].encode(
        tuple._4
      ) + Sql.comma + JdbcEncoder[E].encode(tuple._5) + Sql.comma + JdbcEncoder[F].encode(
        tuple._6
      ) + Sql.comma + JdbcEncoder[G].encode(tuple._7) + Sql.comma + JdbcEncoder[H].encode(
        tuple._8
      ) + Sql.comma + JdbcEncoder[I].encode(tuple._9) + Sql.comma + JdbcEncoder[J].encode(
        tuple._10
      ) + Sql.comma + JdbcEncoder[K].encode(tuple._11)

  // TODO: Other encoders
}
