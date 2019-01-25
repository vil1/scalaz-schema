package scalaz

package schema

object RecursionSchemes {

  trait HFunctor[F[_[_], _]] {
    def hmap[A[_], B[_]](f: A ~> B): F[A, ?] ~> F[B, ?]
  }

  trait HTraverse[F[_[_], _]] extends HFunctor[F] {
    /*
    def traverse[G[_]: Applicative, A[_], B[_]](
      f: A ~> λ[α => G[B[α]]]
    ): F[A, ?] ~> λ[α => G[F[B, α]]]
     */
    def sequence[G[_]: Applicative, A[_]]: F[λ[α => G[A[α]]], ?] ~> λ[α => G[F[A, α]]]
  }

  type HAlgebra[F[_[_], _], G[_]]          = F[G, ?] ~> G
  type HCoalgebra[F[_[_], _], G[_]]        = G ~> F[G, ?]
  type HAlgebraM[M[_], F[_[_], _], G[_]]   = F[G, ?] ~> λ[α => M[G[α]]]
  type HCoalgebraM[M[_], F[_[_], _], G[_]] = G ~> λ[α => M[F[G, α]]]

  def hylo[S[_[_], _], F[_], G[_]](coalgebra: HCoalgebra[S, F], algebra: HAlgebra[S, G])(
    implicit S: HFunctor[S]
  ): F ~> G = new (F ~> G) { self =>

    def apply[A](fa: F[A]): G[A] =
      algebra(S.hmap(self)(coalgebra(fa)))
  }

  def cata[S[_[_], _], F[_]](alg: HAlgebra[S, F])(implicit S: HFunctor[S]): (Fix[S, ?] ~> F) =
    new (Fix[S, ?] ~> F) { self =>

      def apply[A](f: Fix[S, A]): F[A] =
        alg.apply[A](S.hmap(self)(f.unFix))
    }

  def hyloM[M[_], S[_[_], _], F[_], G[_]](
    coalgebra: HCoalgebraM[M, S, F],
    algebra: HAlgebraM[M, S, G]
  )(implicit M: Monad[M], S: HTraverse[S]): F ~> λ[α => M[G[α]]] = {
    type MS[X[_], A] = M[S[X, A]]
    type MG[A]       = M[G[A]]
    hylo[MS, F, MG](coalgebra, new (MS[MG, ?] ~> MG) {

      def apply[A](msmga: M[S[MG, A]]): MG[A] =
        M.bind(msmga)(smga => M.bind(S.sequence[M, G].apply(smga))(algebra.apply[A]))

    })(new HFunctor[MS] {

      def hmap[A[_], B[_]](f: A ~> B) = new (MS[A, ?] ~> MS[B, ?]) {
        def apply[X](msax: MS[A, X]): MS[B, X] =
          M.map(msax)(S.hmap(f))
      }

    })

  }

  def writerCoalg[F[_[_], _]]: HCoalgebraM[State[String, ?], F, Fix[F, ?]] =
    new (Fix[F, ?] ~> λ[α => State[String, F[Fix[F, ?], α]]]) {

      def apply[A](fixFA: Fix[F, A]): State[String, F[Fix[F, ?], A]] =
        State(s => (s ++ s"down: ${fixFA.unFix}\n", fixFA.unFix))
    }

  def writerAlg[F[_[_], _]]: HAlgebraM[State[String, ?], F, Fix[F, ?]] =
    new (F[Fix[F, ?], ?] ~> λ[α => State[String, Fix[F, α]]]) {

      def apply[A](fFixA: F[Fix[F, ?], A]): State[String, Fix[F, A]] =
        State(s => (s ++ s"up: $fFixA\n", Fix(fFixA)))
    }
}
