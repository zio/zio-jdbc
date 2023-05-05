package zio.jdbc

trait Zipper[A, B] {
  type Out
  def combine(a: A, b: B): Out
}

object Zipper extends InvariantZipLowPriority0 {
  type WithOut[A, B, C] = Zipper[A, B] { type Out = C }

  def instance[A, B, C](f: (A, B) => C): Zipper.WithOut[A, B, C] =
    new Zipper[A, B] {
      type Out = C
      def combine(a: A, b: B): C = f(a, b)
    }

  implicit def invariantZipTuple2Unit[A, B]: Zipper.WithOut[(A, B), Unit, (A, B)] =
    instance[(A, B), Unit, (A, B)]((tuple, _) => tuple)

  implicit def invariantZipUnitB[B]: WithOut[Unit, B, B] = new Zipper[Unit, B] {
    type Out = B

    override def combine(a: Unit, b: B): Out = b

  }

  implicit def invariantZipAUnit[A]: WithOut[A, Unit, A] = new Zipper[A, Unit] {
    type Out = A

    override def combine(a: A, b: Unit): Out = a
  }
}

trait InvariantZipLowPriority0 extends InvariantZipLowPriority1 {
  implicit def invariantZipTuple2[A, B, Z]: Zipper.WithOut[(A, B), Z, (A, B, Z)] =
    Zipper.instance[(A, B), Z, (A, B, Z)] { case ((a, b), z) => (a, b, z) }

  implicit def invariantZipTuple3[A, B, C, Z]: Zipper.WithOut[(A, B, C), Z, (A, B, C, Z)] =
    Zipper
      .instance[(A, B, C), Z, (A, B, C, Z)] { case ((a, b, c), z) => (a, b, c, z) }

  implicit def invariantZipTuple4[A, B, C, D, Z]: Zipper.WithOut[(A, B, C, D), Z, (A, B, C, D, Z)] =
    Zipper
      .instance[(A, B, C, D), Z, (A, B, C, D, Z)] { case ((a, b, c, d), z) => (a, b, c, d, z) }

  implicit def invariantZipTuple5[A, B, C, D, E, Z]: Zipper.WithOut[(A, B, C, D, E), Z, (A, B, C, D, E, Z)] =
    Zipper
      .instance[(A, B, C, D, E), Z, (A, B, C, D, E, Z)] { case ((a, b, c, d, e), z) => (a, b, c, d, e, z) }

  implicit def invariantZipTuple6[A, B, C, D, E, F, Z]: Zipper.WithOut[(A, B, C, D, E, F), Z, (A, B, C, D, E, F, Z)] =
    Zipper
      .instance[(A, B, C, D, E, F), Z, (A, B, C, D, E, F, Z)] { case ((a, b, c, d, e, f), z) =>
        (a, b, c, d, e, f, z)
      }

  implicit def invariantZipTuple7[A, B, C, D, E, F, G, Z]
    : Zipper.WithOut[(A, B, C, D, E, F, G), Z, (A, B, C, D, E, F, G, Z)] =
    Zipper
      .instance[(A, B, C, D, E, F, G), Z, (A, B, C, D, E, F, G, Z)] { case ((a, b, c, d, e, f, g), z) =>
        (a, b, c, d, e, f, g, z)
      }

  implicit def invariantZipTuple8[A, B, C, D, E, F, G, H, Z]
    : Zipper.WithOut[(A, B, C, D, E, F, G, H), Z, (A, B, C, D, E, F, G, H, Z)] =
    Zipper
      .instance[(A, B, C, D, E, F, G, H), Z, (A, B, C, D, E, F, G, H, Z)] { case ((a, b, c, d, e, f, g, h), z) =>
        (a, b, c, d, e, f, g, h, z)
      }

  implicit def invariantZipTuple9[A, B, C, D, E, F, G, H, I, Z]
    : Zipper.WithOut[(A, B, C, D, E, F, G, H, I), Z, (A, B, C, D, E, F, G, H, I, Z)] =
    Zipper
      .instance[(A, B, C, D, E, F, G, H, I), Z, (A, B, C, D, E, F, G, H, I, Z)] {
        case ((a, b, c, d, e, f, g, h, i), z) =>
          (a, b, c, d, e, f, g, h, i, z)
      }

  implicit def invariantZipTuple10[A, B, C, D, E, F, G, H, I, J, Z]
    : Zipper.WithOut[(A, B, C, D, E, F, G, H, I, J), Z, (A, B, C, D, E, F, G, H, I, J, Z)] =
    Zipper
      .instance[(A, B, C, D, E, F, G, H, I, J), Z, (A, B, C, D, E, F, G, H, I, J, Z)] {
        case ((a, b, c, d, e, f, g, h, i, j), z) =>
          (a, b, c, d, e, f, g, h, i, j, z)
      }

  implicit def invariantZipTuple11[A, B, C, D, E, F, G, H, I, J, K, Z]
    : Zipper.WithOut[(A, B, C, D, E, F, G, H, I, J, K), Z, (A, B, C, D, E, F, G, H, I, J, K, Z)] =
    Zipper
      .instance[(A, B, C, D, E, F, G, H, I, J, K), Z, (A, B, C, D, E, F, G, H, I, J, K, Z)] {
        case ((a, b, c, d, e, f, g, h, i, j, k), z) =>
          (a, b, c, d, e, f, g, h, i, j, k, z)
      }

  implicit def invariantZipTuple12[A, B, C, D, E, F, G, H, I, J, K, L, Z]
    : Zipper.WithOut[(A, B, C, D, E, F, G, H, I, J, K, L), Z, (A, B, C, D, E, F, G, H, I, J, K, L, Z)] =
    Zipper
      .instance[(A, B, C, D, E, F, G, H, I, J, K, L), Z, (A, B, C, D, E, F, G, H, I, J, K, L, Z)] {
        case ((a, b, c, d, e, f, g, h, i, j, k, l), z) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, z)
      }

  implicit def invariantZipTuple13[A, B, C, D, E, F, G, H, I, J, K, L, M, Z]
    : Zipper.WithOut[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)] =
    Zipper
      .instance[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)] {
        case ((a, b, c, d, e, f, g, h, i, j, k, l, m), z) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, z)
      }

  implicit def invariantZipTuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z]: Zipper.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)
  ] =
    Zipper
      .instance[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)] {
        case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n), z) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, z)
      }

  implicit def invariantZipTuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z]: Zipper.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)
  ] =
    Zipper
      .instance[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)] {
        case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o), z) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, z)
      }

  implicit def invariantZipTuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z]: Zipper.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)
  ] =
    Zipper
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)
      ] { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, z)
      }

  implicit def invariantZipTuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z]: Zipper.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)
  ] =
    Zipper
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)
      ] { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, z)
      }

  implicit def invariantZipTuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z]: Zipper.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)
  ] =
    Zipper
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)
      ] { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, z)
      }

  implicit def invariantZipTuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z]: Zipper.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)
  ] =
    Zipper
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)
      ] { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, z)
      }

  implicit def invariantZipTuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z]: Zipper.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)
  ] =
    Zipper
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)
      ] { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, z)
      }

  implicit def invariantZipTuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z]: Zipper.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)
  ] =
    Zipper
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)
      ] { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, z)
      }

}

trait InvariantZipLowPriority1 {
  implicit def invariantZipAB[A, B]: Zipper.WithOut[A, B, (A, B)] = new Zipper[A, B] {
    type Out = (A, B)

    override def combine(a: A, b: B): (A, B) = ((a, b))
  }
}
