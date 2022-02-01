package zio.jdbc

trait JdbcEncoder[-A] {
  def encode(value: A): SqlStatement[ZResultSet]

  final def contramap[B](f: B => A): JdbcEncoder[B] = (value) => encode(f(value))
}
object JdbcEncoder    {
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
}
