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

import zio.Chunk
import zio.schema.{ Schema, StandardType }

/**
 * A type class that describes the ability to convert a value of type `A` into
 * a fragment of SQL. This is useful for forming SQL insert statements.
 */
trait JdbcEncoder[-A] {
  def encode(value: A): SqlFragment

  final def contramap[B](f: B => A): JdbcEncoder[B] = value => encode(f(value))
}

object JdbcEncoder extends JdbcEncoderLowPriorityImplicits {
  def apply[A]()(implicit encoder: JdbcEncoder[A]): JdbcEncoder[A] = encoder

  implicit val intEncoder: JdbcEncoder[Int]                               = value => sql"$value"
  implicit val longEncoder: JdbcEncoder[Long]                             = value => sql"$value"
  implicit val doubleEncoder: JdbcEncoder[Double]                         = value => sql"$value"
  implicit val charEncoder: JdbcEncoder[Char]                             = value => sql"$value"
  implicit val stringEncoder: JdbcEncoder[String]                         = value => sql"$value"
  implicit val booleanEncoder: JdbcEncoder[Boolean]                       = value => sql"$value"
  implicit val bigIntDecoder: JdbcEncoder[java.math.BigInteger]           = value => sql"$value"
  implicit val bigDecimalEncoder: JdbcEncoder[java.math.BigDecimal]       = value => sql"$value"
  implicit val bigDecimalEncoderScala: JdbcEncoder[scala.math.BigDecimal] = value => sql"$value"
  implicit val shortEncoder: JdbcEncoder[Short]                           = value => sql"$value"
  implicit val floatEncoder: JdbcEncoder[Float]                           = value => sql"$value"
  implicit val byteEncoder: JdbcEncoder[Byte]                             = value => sql"$value"
  implicit val byteArrayEncoder: JdbcEncoder[Array[Byte]]                 = value => sql"$value"
  implicit val byteChunkEncoder: JdbcEncoder[Chunk[Byte]]                 = value => sql"$value"
  implicit val blobEncoder: JdbcEncoder[java.sql.Blob]                    = value => sql"$value"
  implicit val uuidEncoder: JdbcEncoder[java.util.UUID]                   = value => sql"$value"

  implicit def singleParamEncoder[A: Sql.ParamSetter]: JdbcEncoder[A] = value => sql"$value"

  implicit def optionEncoder[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[Option[A]] =
    value => value.fold(Sql.nullLiteral)(encoder.encode)

  implicit def tuple2Encoder[A: JdbcEncoder, B: JdbcEncoder]: JdbcEncoder[(A, B)] =
    tuple => JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(tuple._2)

  implicit def tuple3Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder]: JdbcEncoder[(A, B, C)] =
    tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3)

  implicit def tuple4Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder, D: JdbcEncoder]
    : JdbcEncoder[(A, B, C, D)] =
    tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(tuple._4)

  implicit def tuple5Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder, D: JdbcEncoder, E: JdbcEncoder]
    : JdbcEncoder[(A, B, C, D, E)] =
    tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5)

  implicit def tuple6Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F)] =
    tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(tuple._6)

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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7)

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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(tuple._8)

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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9)

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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(tuple._10)

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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11)

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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L]().encode(
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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M]().encode(tuple._13)

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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N]().encode(
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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O]().encode(tuple._15)

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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P]().encode(
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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q]().encode(tuple._17)

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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q]().encode(tuple._17) ++ Sql.comma ++ JdbcEncoder[R]().encode(
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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q]().encode(tuple._17) ++ Sql.comma ++ JdbcEncoder[R]().encode(
        tuple._18
      ) ++ Sql.comma ++ JdbcEncoder[S]().encode(tuple._19)

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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q]().encode(tuple._17) ++ Sql.comma ++ JdbcEncoder[R]().encode(
        tuple._18
      ) ++ Sql.comma ++ JdbcEncoder[S]().encode(tuple._19) ++ Sql.comma ++ JdbcEncoder[T]().encode(
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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q]().encode(tuple._17) ++ Sql.comma ++ JdbcEncoder[R]().encode(
        tuple._18
      ) ++ Sql.comma ++ JdbcEncoder[S]().encode(tuple._19) ++ Sql.comma ++ JdbcEncoder[T]().encode(
        tuple._20
      ) ++ Sql.comma ++ JdbcEncoder[U]().encode(tuple._21)

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
      JdbcEncoder[A]().encode(tuple._1) ++ Sql.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ Sql.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ Sql.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ Sql.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ Sql.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ Sql.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ Sql.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ Sql.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ Sql.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ Sql.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ Sql.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ Sql.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ Sql.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ Sql.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ Sql.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ Sql.comma ++ JdbcEncoder[Q]().encode(tuple._17) ++ Sql.comma ++ JdbcEncoder[R]().encode(
        tuple._18
      ) ++ Sql.comma ++ JdbcEncoder[S]().encode(tuple._19) ++ Sql.comma ++ JdbcEncoder[T]().encode(
        tuple._20
      ) ++ Sql.comma ++ JdbcEncoder[U]().encode(tuple._21) ++ Sql.comma ++ JdbcEncoder[V]().encode(
        tuple._22
      )
}

trait JdbcEncoderLowPriorityImplicits { self =>
  private[jdbc] def primitiveCodec[A](standardType: StandardType[A]): JdbcEncoder[A] =
    standardType match {
      case StandardType.StringType     => JdbcEncoder.stringEncoder
      case StandardType.BoolType       => JdbcEncoder.booleanEncoder
      case StandardType.ShortType      => JdbcEncoder.shortEncoder
      case StandardType.IntType        => JdbcEncoder.intEncoder
      case StandardType.LongType       => JdbcEncoder.longEncoder
      case StandardType.FloatType      => JdbcEncoder.floatEncoder
      case StandardType.DoubleType     => JdbcEncoder.doubleEncoder
      case StandardType.CharType       => JdbcEncoder.charEncoder
      case StandardType.BigIntegerType => JdbcEncoder.bigIntDecoder
      case StandardType.BinaryType     => JdbcEncoder.byteChunkEncoder
      case StandardType.BigDecimalType => JdbcEncoder.bigDecimalEncoder
      case StandardType.UUIDType       => JdbcEncoder.uuidEncoder
      // TODO: Standard Types which are missing are the date time types, not sure what would be the best way to handle them
      case _                           => throw JdbcEncoderError(s"Unsupported type: $standardType", new IllegalArgumentException)
    }

  //scalafmt: { maxColumn = 325, optIn.configStyleArguments = false }
  def fromSchema[A](implicit schema: Schema[A]): JdbcEncoder[A] =
    schema match {
      case Schema.Primitive(standardType, _)                                                                                                                                                                                                                                                 =>
        primitiveCodec(standardType)
      case Schema.Optional(schema, _)                                                                                                                                                                                                                                                        =>
        JdbcEncoder.optionEncoder(self.fromSchema(schema))
      case Schema.Tuple(left, right, _)                                                                                                                                                                                                                                                      =>
        JdbcEncoder.tuple2Encoder(self.fromSchema(left), self.fromSchema(right))
      case Schema.CaseClass1(_, f, _, ext, _)                                                                                                                                                                                                                                                =>
        caseClassEncoder(f -> ext)
      case Schema.CaseClass2(_, f1, f2, _, ext1, ext2, _)                                                                                                                                                                                                                                    =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2)
      case Schema.CaseClass3(_, f1, f2, f3, _, ext1, ext2, ext3, _)                                                                                                                                                                                                                          =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3)
      case Schema.CaseClass4(_, f1, f2, f3, f4, _, ext1, ext2, ext3, ext4, _)                                                                                                                                                                                                                =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4)
      case Schema.CaseClass5(_, f1, f2, f3, f4, f5, _, ext1, ext2, ext3, ext4, ext5, _)                                                                                                                                                                                                      =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5)
      case Schema.CaseClass6(_, f1, f2, f3, f4, f5, f6, _, ext1, ext2, ext3, ext4, ext5, ext6, _)                                                                                                                                                                                            =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6)
      case Schema.CaseClass7(_, f1, f2, f3, f4, f5, f6, f7, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, _)                                                                                                                                                                                  =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7)
      case Schema.CaseClass8(_, f1, f2, f3, f4, f5, f6, f7, f8, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, _)                                                                                                                                                                        =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8)
      case Schema.CaseClass9(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, _)                                                                                                                                                              =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9)
      case Schema.CaseClass10(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, _)                                                                                                                                                 =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10)
      case Schema.CaseClass11(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, _)                                                                                                                                     =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11)
      case Schema.CaseClass12(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, _)                                                                                                                         =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12)
      case Schema.CaseClass13(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, _)                                                                                                             =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13)
      case Schema.CaseClass14(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, _)                                                                                                 =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14)
      case Schema.CaseClass15(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, _)                                                                                     =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15)
      case Schema.CaseClass16(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, _)                                                                         =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16)
      case Schema.CaseClass17(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, _)                                                             =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17)
      case Schema.CaseClass18(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, _)                                                 =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18)
      case Schema.CaseClass19(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, _)                                     =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19)
      case Schema.CaseClass20(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20, _)                         =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20)
      case Schema.CaseClass21(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20, ext21, _)             =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20, f21 -> ext21)
      case Schema.CaseClass22(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20, ext21, ext22, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20, f21 -> ext21, f22 -> ext22)
      case _                                                                                                                                                                                                                                                                                 =>
        throw JdbcEncoderError(s"Failed to encode schema ${schema}", new IllegalArgumentException)
    }

  private[jdbc] def caseClassEncoder[A](fields: (Schema.Field[_], A => Any)*): JdbcEncoder[A] = { (a: A) =>
    fields.map { case (Schema.Field(_, schema, _, _), extractor) =>
      val encoder = self.fromSchema(schema).asInstanceOf[JdbcEncoder[Any]]
      encoder.encode(extractor(a))
    }.reduce(_ ++ Sql.comma ++ _)
  }
}
