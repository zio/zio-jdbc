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
trait JdbcEncoder0[-A] {
  def encode(value: A): SqlFragment0

  final def contramap[B](f: B => A): JdbcEncoder0[B] = value => encode(f(value))
}

object JdbcEncoder0 extends JdbcEncoder0LowPriorityImplicits {
  def apply[A]()(implicit encoder: JdbcEncoder0[A]): JdbcEncoder0[A] = encoder

  implicit val intEncoder: JdbcEncoder0[Int]                               = value => sql0"$value"
  implicit val longEncoder: JdbcEncoder0[Long]                             = value => sql0"$value"
  implicit val doubleEncoder: JdbcEncoder0[Double]                         = value => sql0"$value"
  implicit val charEncoder: JdbcEncoder0[Char]                             = value => sql0"$value"
  implicit val stringEncoder: JdbcEncoder0[String]                         = value => sql0"$value"
  implicit val booleanEncoder: JdbcEncoder0[Boolean]                       = value => sql0"$value"
  implicit val bigIntEncoder: JdbcEncoder0[java.math.BigInteger]           = value => sql0"$value"
  implicit val bigDecimalEncoder: JdbcEncoder0[java.math.BigDecimal]       = value => sql0"$value"
  implicit val bigDecimalEncoderScala: JdbcEncoder0[scala.math.BigDecimal] = value => sql0"$value"
  implicit val shortEncoder: JdbcEncoder0[Short]                           = value => sql0"$value"
  implicit val floatEncoder: JdbcEncoder0[Float]                           = value => sql0"$value"
  implicit val byteEncoder: JdbcEncoder0[Byte]                             = value => sql0"$value"
  implicit val byteArrayEncoder: JdbcEncoder0[Array[Byte]]                 = value => sql0"$value"
  implicit val byteChunkEncoder: JdbcEncoder0[Chunk[Byte]]                 = value => sql0"$value"
  implicit val blobEncoder: JdbcEncoder0[java.sql.Blob]                    = value => sql0"$value"
  implicit val uuidEncoder: JdbcEncoder0[java.util.UUID]                   = value => sql0"$value"

  implicit def singleParamEncoder[A: SqlFragment0.Setter]: JdbcEncoder0[A] = value => sql0"$value"

  // TODO: review for cases like Option of a tuple
  def optionEncoder[A](implicit encoder: JdbcEncoder0[A]): JdbcEncoder0[Option[A]] =
    value => value.fold(SqlFragment0.nullLiteral)(encoder.encode)

  implicit def tuple2Encoder[A: JdbcEncoder0, B: JdbcEncoder0]: JdbcEncoder0[(A, B)] =
    tuple => JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(tuple._2)

  implicit def tuple3Encoder[A: JdbcEncoder0, B: JdbcEncoder0, C: JdbcEncoder0]: JdbcEncoder0[(A, B, C)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3)

  implicit def tuple4Encoder[A: JdbcEncoder0, B: JdbcEncoder0, C: JdbcEncoder0, D: JdbcEncoder0]
    : JdbcEncoder0[(A, B, C, D)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      )

  implicit def tuple5Encoder[A: JdbcEncoder0, B: JdbcEncoder0, C: JdbcEncoder0, D: JdbcEncoder0, E: JdbcEncoder0]
    : JdbcEncoder0[(A, B, C, D, E)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5)

  implicit def tuple6Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      )

  implicit def tuple7Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7)

  implicit def tuple8Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      )

  implicit def tuple9Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9)

  implicit def tuple10Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      )

  implicit def tuple11Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11)

  implicit def tuple12Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0,
    L: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11) ++ SqlFragment0.comma ++ JdbcEncoder0[L]().encode(
        tuple._12
      )

  implicit def tuple13Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0,
    L: JdbcEncoder0,
    M: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11) ++ SqlFragment0.comma ++ JdbcEncoder0[L]().encode(
        tuple._12
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[M]().encode(tuple._13)

  implicit def tuple14Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0,
    L: JdbcEncoder0,
    M: JdbcEncoder0,
    N: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11) ++ SqlFragment0.comma ++ JdbcEncoder0[L]().encode(
        tuple._12
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[M]().encode(tuple._13) ++ SqlFragment0.comma ++ JdbcEncoder0[N]().encode(
        tuple._14
      )

  implicit def tuple15Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0,
    L: JdbcEncoder0,
    M: JdbcEncoder0,
    N: JdbcEncoder0,
    O: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11) ++ SqlFragment0.comma ++ JdbcEncoder0[L]().encode(
        tuple._12
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[M]().encode(tuple._13) ++ SqlFragment0.comma ++ JdbcEncoder0[N]().encode(
        tuple._14
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[O]().encode(tuple._15)

  implicit def tuple16Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0,
    L: JdbcEncoder0,
    M: JdbcEncoder0,
    N: JdbcEncoder0,
    O: JdbcEncoder0,
    P: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11) ++ SqlFragment0.comma ++ JdbcEncoder0[L]().encode(
        tuple._12
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[M]().encode(tuple._13) ++ SqlFragment0.comma ++ JdbcEncoder0[N]().encode(
        tuple._14
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[O]().encode(tuple._15) ++ SqlFragment0.comma ++ JdbcEncoder0[P]().encode(
        tuple._16
      )

  implicit def tuple17Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0,
    L: JdbcEncoder0,
    M: JdbcEncoder0,
    N: JdbcEncoder0,
    O: JdbcEncoder0,
    P: JdbcEncoder0,
    Q: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11) ++ SqlFragment0.comma ++ JdbcEncoder0[L]().encode(
        tuple._12
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[M]().encode(tuple._13) ++ SqlFragment0.comma ++ JdbcEncoder0[N]().encode(
        tuple._14
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[O]().encode(tuple._15) ++ SqlFragment0.comma ++ JdbcEncoder0[P]().encode(
        tuple._16
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[Q]().encode(tuple._17)

  implicit def tuple18Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0,
    L: JdbcEncoder0,
    M: JdbcEncoder0,
    N: JdbcEncoder0,
    O: JdbcEncoder0,
    P: JdbcEncoder0,
    Q: JdbcEncoder0,
    R: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11) ++ SqlFragment0.comma ++ JdbcEncoder0[L]().encode(
        tuple._12
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[M]().encode(tuple._13) ++ SqlFragment0.comma ++ JdbcEncoder0[N]().encode(
        tuple._14
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[O]().encode(tuple._15) ++ SqlFragment0.comma ++ JdbcEncoder0[P]().encode(
        tuple._16
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[Q]().encode(tuple._17) ++ SqlFragment0.comma ++ JdbcEncoder0[R]().encode(
        tuple._18
      )

  implicit def tuple19Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0,
    L: JdbcEncoder0,
    M: JdbcEncoder0,
    N: JdbcEncoder0,
    O: JdbcEncoder0,
    P: JdbcEncoder0,
    Q: JdbcEncoder0,
    R: JdbcEncoder0,
    S: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11) ++ SqlFragment0.comma ++ JdbcEncoder0[L]().encode(
        tuple._12
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[M]().encode(tuple._13) ++ SqlFragment0.comma ++ JdbcEncoder0[N]().encode(
        tuple._14
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[O]().encode(tuple._15) ++ SqlFragment0.comma ++ JdbcEncoder0[P]().encode(
        tuple._16
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[Q]().encode(tuple._17) ++ SqlFragment0.comma ++ JdbcEncoder0[R]().encode(
        tuple._18
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[S]().encode(tuple._19)

  implicit def tuple20Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0,
    L: JdbcEncoder0,
    M: JdbcEncoder0,
    N: JdbcEncoder0,
    O: JdbcEncoder0,
    P: JdbcEncoder0,
    Q: JdbcEncoder0,
    R: JdbcEncoder0,
    S: JdbcEncoder0,
    T: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11) ++ SqlFragment0.comma ++ JdbcEncoder0[L]().encode(
        tuple._12
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[M]().encode(tuple._13) ++ SqlFragment0.comma ++ JdbcEncoder0[N]().encode(
        tuple._14
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[O]().encode(tuple._15) ++ SqlFragment0.comma ++ JdbcEncoder0[P]().encode(
        tuple._16
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[Q]().encode(tuple._17) ++ SqlFragment0.comma ++ JdbcEncoder0[R]().encode(
        tuple._18
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[S]().encode(tuple._19) ++ SqlFragment0.comma ++ JdbcEncoder0[T]().encode(
        tuple._20
      )

  implicit def tuple21Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0,
    L: JdbcEncoder0,
    M: JdbcEncoder0,
    N: JdbcEncoder0,
    O: JdbcEncoder0,
    P: JdbcEncoder0,
    Q: JdbcEncoder0,
    R: JdbcEncoder0,
    S: JdbcEncoder0,
    T: JdbcEncoder0,
    U: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11) ++ SqlFragment0.comma ++ JdbcEncoder0[L]().encode(
        tuple._12
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[M]().encode(tuple._13) ++ SqlFragment0.comma ++ JdbcEncoder0[N]().encode(
        tuple._14
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[O]().encode(tuple._15) ++ SqlFragment0.comma ++ JdbcEncoder0[P]().encode(
        tuple._16
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[Q]().encode(tuple._17) ++ SqlFragment0.comma ++ JdbcEncoder0[R]().encode(
        tuple._18
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[S]().encode(tuple._19) ++ SqlFragment0.comma ++ JdbcEncoder0[T]().encode(
        tuple._20
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[U]().encode(tuple._21)

  implicit def tuple22Encoder[
    A: JdbcEncoder0,
    B: JdbcEncoder0,
    C: JdbcEncoder0,
    D: JdbcEncoder0,
    E: JdbcEncoder0,
    F: JdbcEncoder0,
    G: JdbcEncoder0,
    H: JdbcEncoder0,
    I: JdbcEncoder0,
    J: JdbcEncoder0,
    K: JdbcEncoder0,
    L: JdbcEncoder0,
    M: JdbcEncoder0,
    N: JdbcEncoder0,
    O: JdbcEncoder0,
    P: JdbcEncoder0,
    Q: JdbcEncoder0,
    R: JdbcEncoder0,
    S: JdbcEncoder0,
    T: JdbcEncoder0,
    U: JdbcEncoder0,
    V: JdbcEncoder0
  ]: JdbcEncoder0[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    tuple =>
      JdbcEncoder0[A]().encode(tuple._1) ++ SqlFragment0.comma ++ JdbcEncoder0[B]().encode(
        tuple._2
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[C]().encode(tuple._3) ++ SqlFragment0.comma ++ JdbcEncoder0[D]().encode(
        tuple._4
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[E]().encode(tuple._5) ++ SqlFragment0.comma ++ JdbcEncoder0[F]().encode(
        tuple._6
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[G]().encode(tuple._7) ++ SqlFragment0.comma ++ JdbcEncoder0[H]().encode(
        tuple._8
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[I]().encode(tuple._9) ++ SqlFragment0.comma ++ JdbcEncoder0[J]().encode(
        tuple._10
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[K]().encode(tuple._11) ++ SqlFragment0.comma ++ JdbcEncoder0[L]().encode(
        tuple._12
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[M]().encode(tuple._13) ++ SqlFragment0.comma ++ JdbcEncoder0[N]().encode(
        tuple._14
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[O]().encode(tuple._15) ++ SqlFragment0.comma ++ JdbcEncoder0[P]().encode(
        tuple._16
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[Q]().encode(tuple._17) ++ SqlFragment0.comma ++ JdbcEncoder0[R]().encode(
        tuple._18
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[S]().encode(tuple._19) ++ SqlFragment0.comma ++ JdbcEncoder0[T]().encode(
        tuple._20
      ) ++ SqlFragment0.comma ++ JdbcEncoder0[U]().encode(tuple._21) ++ SqlFragment0.comma ++ JdbcEncoder0[V]().encode(
        tuple._22
      )
}

trait JdbcEncoder0LowPriorityImplicits { self =>
  private[jdbc] def primitiveCodec[A](standardType: StandardType[A]): JdbcEncoder0[A] =
    standardType match {
      case StandardType.StringType     => JdbcEncoder0.stringEncoder
      case StandardType.BoolType       => JdbcEncoder0.booleanEncoder
      case StandardType.ShortType      => JdbcEncoder0.shortEncoder
      case StandardType.IntType        => JdbcEncoder0.intEncoder
      case StandardType.LongType       => JdbcEncoder0.longEncoder
      case StandardType.FloatType      => JdbcEncoder0.floatEncoder
      case StandardType.DoubleType     => JdbcEncoder0.doubleEncoder
      case StandardType.CharType       => JdbcEncoder0.charEncoder
      case StandardType.BigIntegerType => JdbcEncoder0.bigIntEncoder
      case StandardType.BinaryType     => JdbcEncoder0.byteChunkEncoder
      case StandardType.BigDecimalType => JdbcEncoder0.bigDecimalEncoder
      case StandardType.UUIDType       => JdbcEncoder0.uuidEncoder
      // TODO: Standard Types which are missing are the date time types, not sure what would be the best way to handle them
      case _                           => throw JdbcEncoderError(s"Unsupported type: $standardType", new IllegalArgumentException)
    }

  //scalafmt: { maxColumn = 325, optIn.configStyleArguments = false }
  def fromSchema[A](implicit schema: Schema[A]): JdbcEncoder0[A] =
    schema match {
      case Schema.Primitive(standardType, _) =>
        primitiveCodec(standardType)
      case Schema.Optional(schema, _)        =>
        JdbcEncoder0.optionEncoder(self.fromSchema(schema))
      case Schema.Tuple2(left, right, _)     =>
        JdbcEncoder0.tuple2Encoder(self.fromSchema(left), self.fromSchema(right))
      // format: off
      case x@(
        _: Schema.CaseClass1[_, _] |
        _: Schema.CaseClass2[_, _, _] |
        _: Schema.CaseClass3[_, _, _, _] |
        _: Schema.CaseClass4[_, _, _, _, _] |
        _: Schema.CaseClass5[_, _, _, _, _, _] |
        _: Schema.CaseClass6[_, _, _, _, _, _, _] |
        _: Schema.CaseClass7[_, _, _, _, _, _, _, _] |
        _: Schema.CaseClass8[_, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        ) =>
        // format: on
        caseClassEncoder(x.asInstanceOf[Schema.Record[A]].fields)
      case _                                 =>
        throw JdbcEncoderError(s"Failed to encode schema ${schema}", new IllegalArgumentException)
    }

  private[jdbc] def caseClassEncoder[A](fields: Chunk[Schema.Field[A, _]]): JdbcEncoder0[A] = { (a: A) =>
    fields.map { f =>
      val encoder = self.fromSchema(f.schema.asInstanceOf[Schema[Any]])
      encoder.encode(f.get(a))
    }.reduce(_ ++ SqlFragment0.comma ++ _)
  }
}
