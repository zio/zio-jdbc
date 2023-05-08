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
import zio.jdbc.SqlFragment.{ Segment, Setter }
import zio.schema.{ Schema, StandardType }

import java.sql.PreparedStatement

/**
 * A type class that describes the ability to convert a value of type `A` into
 * a fragment of SQL. This is useful for forming SQL insert statements.
 */
trait JdbcEncoder[A] {
  def encode(value: A): SqlFragment
  val setter: Option[Setter[A]]

  final def contramap[B](f: B => A): JdbcEncoder[B] = {
    val that = this
    new JdbcEncoder[B] {
      override def encode(value: B): SqlFragment = that.encode(f(value))

      override val setter: Option[Setter[B]] = that.setter.map(_.contramap(f))
    }
  }
}

object JdbcEncoder extends JdbcEncoder0LowPriorityImplicits {
  def apply[A]()(implicit encoder: JdbcEncoder[A]): JdbcEncoder[A] = encoder

  /**
   * Use caution when using this method. Returning interpolating string may result in SQL injection attacks
   */
  def apply[A](onEncode: A => SqlFragment): JdbcEncoder[A] = new JdbcEncoder[A] {
    override def encode(value: A): SqlFragment = onEncode(value)

    override val setter: Option[Setter[A]] = None
  }

  def apply[A](
    onEncode: A => SqlFragment,
    onValue: (PreparedStatement, Int, A) => Unit,
    onNull: (PreparedStatement, Int) => Unit
  ): JdbcEncoder[A] = new JdbcEncoder[A] {
    override def encode(value: A): SqlFragment = onEncode(value)

    override val setter: Option[Setter[A]] = Some(new Setter[A] {
      override def unsafeSetValue(ps: PreparedStatement, index: Int, value: A): Unit = onValue(ps, index, value)

      override def unsafeSetNull(ps: PreparedStatement, index: Int): Unit = onNull(ps, index)
    })
  }

  private def withSetter[A](implicit setter: Setter[A]): JdbcEncoder[A] =
    apply(
      value => SqlFragment(Chunk.apply(Segment.Param(value, setter.asInstanceOf[Setter[Any]]))),
      setter.unsafeSetValue,
      setter.unsafeSetNull
    )

  implicit val intEncoder: JdbcEncoder[Int]                               = withSetter
  implicit val longEncoder: JdbcEncoder[Long]                             = withSetter
  implicit val doubleEncoder: JdbcEncoder[Double]                         = withSetter
  implicit val charEncoder: JdbcEncoder[Char]                             = withSetter
  implicit val stringEncoder: JdbcEncoder[String]                         = withSetter
  implicit val booleanEncoder: JdbcEncoder[Boolean]                       = withSetter
  implicit val bigIntEncoder: JdbcEncoder[java.math.BigInteger]           = withSetter
  implicit val bigDecimalEncoder: JdbcEncoder[java.math.BigDecimal]       = withSetter
  implicit val bigDecimalEncoderScala: JdbcEncoder[scala.math.BigDecimal] = withSetter
  implicit val shortEncoder: JdbcEncoder[Short]                           = withSetter
  implicit val floatEncoder: JdbcEncoder[Float]                           = withSetter
  implicit val byteEncoder: JdbcEncoder[Byte]                             = withSetter
  implicit val byteArrayEncoder: JdbcEncoder[Array[Byte]]                 = withSetter
  implicit val byteChunkEncoder: JdbcEncoder[Chunk[Byte]]                 = withSetter
  implicit val blobEncoder: JdbcEncoder[java.sql.Blob]                    = withSetter
  implicit val uuidEncoder: JdbcEncoder[java.util.UUID]                   = withSetter

  private def optionParamSetter[A](implicit setter: Setter[A]): Setter[Option[A]]         =
    Setter(
      (ps, i, value) =>
        value match {
          case Some(value) => setter.unsafeSetValue(ps, i, value)
          case None        => setter.unsafeSetNull(ps, i)
        },
      (ps, i) => setter.unsafeSetNull(ps, i)
    )
  implicit def optionEncoder[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[Option[A]] = new JdbcEncoder[Option[A]] {
    override def encode(value: Option[A]): SqlFragment = value.fold(SqlFragment.nullLiteral)(encoder.encode)

    override val setter: Option[Setter[Option[A]]] = encoder.setter.map(optionParamSetter(_))
  }

  implicit def tuple2Encoder[A: JdbcEncoder, B: JdbcEncoder]: JdbcEncoder[(A, B)] =
    JdbcEncoder(tuple => JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(tuple._2))

  implicit def tuple3Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder]: JdbcEncoder[(A, B, C)] =
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3)
    )

  implicit def tuple4Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder, D: JdbcEncoder]
    : JdbcEncoder[(A, B, C, D)] =
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      )
    )

  implicit def tuple5Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder, D: JdbcEncoder, E: JdbcEncoder]
    : JdbcEncoder[(A, B, C, D, E)] =
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5)
    )

  implicit def tuple6Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F)] =
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      )
    )

  implicit def tuple7Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G)] =
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7)
    )

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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      )
    )

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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9)
    )

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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      )
    )

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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11)
    )

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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      )
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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M]().encode(tuple._13)
    )

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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      )
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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O]().encode(tuple._15)
    )

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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      )
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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q]().encode(tuple._17)
    )

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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q]().encode(tuple._17) ++ SqlFragment.comma ++ JdbcEncoder[R]().encode(
        tuple._18
      )
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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q]().encode(tuple._17) ++ SqlFragment.comma ++ JdbcEncoder[R]().encode(
        tuple._18
      ) ++ SqlFragment.comma ++ JdbcEncoder[S]().encode(tuple._19)
    )

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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q]().encode(tuple._17) ++ SqlFragment.comma ++ JdbcEncoder[R]().encode(
        tuple._18
      ) ++ SqlFragment.comma ++ JdbcEncoder[S]().encode(tuple._19) ++ SqlFragment.comma ++ JdbcEncoder[T]().encode(
        tuple._20
      )
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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q]().encode(tuple._17) ++ SqlFragment.comma ++ JdbcEncoder[R]().encode(
        tuple._18
      ) ++ SqlFragment.comma ++ JdbcEncoder[S]().encode(tuple._19) ++ SqlFragment.comma ++ JdbcEncoder[T]().encode(
        tuple._20
      ) ++ SqlFragment.comma ++ JdbcEncoder[U]().encode(tuple._21)
    )

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
    JdbcEncoder(tuple =>
      JdbcEncoder[A]().encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B]().encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C]().encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D]().encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E]().encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F]().encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G]().encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H]().encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I]().encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J]().encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K]().encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L]().encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M]().encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N]().encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O]().encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P]().encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q]().encode(tuple._17) ++ SqlFragment.comma ++ JdbcEncoder[R]().encode(
        tuple._18
      ) ++ SqlFragment.comma ++ JdbcEncoder[S]().encode(tuple._19) ++ SqlFragment.comma ++ JdbcEncoder[T]().encode(
        tuple._20
      ) ++ SqlFragment.comma ++ JdbcEncoder[U]().encode(tuple._21) ++ SqlFragment.comma ++ JdbcEncoder[V]().encode(
        tuple._22
      )
    )
}

trait JdbcEncoder0LowPriorityImplicits { self =>
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
      case StandardType.BigIntegerType => JdbcEncoder.bigIntEncoder
      case StandardType.BinaryType     => JdbcEncoder.byteChunkEncoder
      case StandardType.BigDecimalType => JdbcEncoder.bigDecimalEncoder
      case StandardType.UUIDType       => JdbcEncoder.uuidEncoder
      // TODO: Standard Types which are missing are the date time types, not sure what would be the best way to handle them
      case _                           => throw JdbcEncoderError(s"Unsupported type: $standardType", new IllegalArgumentException)
    }

  //scalafmt: { maxColumn = 325, optIn.configStyleArguments = false }
  def fromSchema[A](implicit schema: Schema[A]): JdbcEncoder[A] =
    schema match {
      case Schema.Primitive(standardType, _) =>
        primitiveCodec(standardType)
      case Schema.Optional(schema, _)        =>
        JdbcEncoder.optionEncoder(self.fromSchema(schema))
      case Schema.Tuple2(left, right, _)     =>
        JdbcEncoder.tuple2Encoder(self.fromSchema(left), self.fromSchema(right))
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

  private[jdbc] def caseClassEncoder[A](fields: Chunk[Schema.Field[A, _]]): JdbcEncoder[A] = JdbcEncoder(a =>
    fields.map { f =>
      val encoder = self.fromSchema(f.schema.asInstanceOf[Schema[Any]])
      encoder.encode(f.get(a))
    }.reduce(_ ++ SqlFragment.comma ++ _)
  )

}
