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

/**
 * A type class that describes the ability to convert a value of type `A` into
 * a fragment of SQL. This is useful for forming SQL insert statements.
 */
trait JdbcEncoder[-A] {
  def encode(value: A): Sql[ZResultSet]

  final def contramap[B](f: B => A): JdbcEncoder[B] = (value) => encode(f(value))
}

object JdbcEncoder {
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
    value => value.fold(Sql.nullLiteral)(encoder.encode(_))

  implicit def tuple2Encoder[A: JdbcEncoder, B: JdbcEncoder]: JdbcEncoder[(A, B)] =
    tuple => JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(tuple._2)

  implicit def tuple3Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder]: JdbcEncoder[(A, B, C)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3)

  implicit def tuple4Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder, D: JdbcEncoder]
    : JdbcEncoder[(A, B, C, D)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(tuple._4)

  implicit def tuple5Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder, D: JdbcEncoder, E: JdbcEncoder]
    : JdbcEncoder[(A, B, C, D, E)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5)

  implicit def tuple6Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(tuple._6)

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
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7)

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
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(tuple._8)

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
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9)

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
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(tuple._10)

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
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11)

  implicit def tuple12Encoder[
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
    K: JdbcEncoder,
    L: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L].encode(
        tuple._12
      )

  implicit def tuple13Encoder[
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
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M].encode(tuple._13)

  implicit def tuple14Encoder[
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
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M].encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N].encode(
        tuple._14
      )

  implicit def tuple15Encoder[
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
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M].encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O].encode(tuple._15)

  implicit def tuple16Encoder[
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
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M].encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O].encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P].encode(
        tuple._16
      )

  implicit def tuple17Encoder[
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
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M].encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O].encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q].encode(tuple._17)

  implicit def tuple18Encoder[
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
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder,
    R: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M].encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O].encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q].encode(tuple._17) ++ Sql.comma ++ JdbcEncoder[R].encode(
        tuple._18
      )

  implicit def tuple19Encoder[
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
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder,
    R: JdbcEncoder,
    S: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M].encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O].encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q].encode(tuple._17) ++ Sql.comma ++ JdbcEncoder[R].encode(
        tuple._18
      ) ++ Sql.comma ++ JdbcEncoder[S].encode(tuple._19)

  implicit def tuple20Encoder[
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
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder,
    R: JdbcEncoder,
    S: JdbcEncoder,
    T: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M].encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O].encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q].encode(tuple._17) ++ Sql.comma ++ JdbcEncoder[R].encode(
        tuple._18
      ) ++ Sql.comma ++ JdbcEncoder[S].encode(tuple._19) ++ Sql.comma ++ JdbcEncoder[T].encode(
        tuple._20
      )

  implicit def tuple21Encoder[
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
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder,
    R: JdbcEncoder,
    S: JdbcEncoder,
    T: JdbcEncoder,
    U: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M].encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O].encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q].encode(tuple._17) ++ Sql.comma ++ JdbcEncoder[R].encode(
        tuple._18
      ) ++ Sql.comma ++ JdbcEncoder[S].encode(tuple._19) ++ Sql.comma ++ JdbcEncoder[T].encode(
        tuple._20
      ) ++ Sql.comma ++ JdbcEncoder[U].encode(tuple._21)

  implicit def tuple22Encoder[
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
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder,
    R: JdbcEncoder,
    S: JdbcEncoder,
    T: JdbcEncoder,
    U: JdbcEncoder,
    V: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C].encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E].encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G].encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I].encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K].encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M].encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O].encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q].encode(tuple._17) ++ Sql.comma ++ JdbcEncoder[R].encode(
        tuple._18
      ) ++ Sql.comma ++ JdbcEncoder[S].encode(tuple._19) ++ Sql.comma ++ JdbcEncoder[T].encode(
        tuple._20
      ) ++ Sql.comma ++ JdbcEncoder[U].encode(tuple._21) ++ Sql.comma ++ JdbcEncoder[V].encode(
        tuple._22
      )

}
