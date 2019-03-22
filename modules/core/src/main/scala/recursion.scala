package scalaz

package schema

import scalaz.{ BiNaturalTransformation => ~>> }

package recursion {

  trait HFunctor[H[_[_], _]] {
    def hmap[F[_], G[_]](nt: F ~> G): H[F, ?] ~> H[G, ?]
  }

  trait HBiFunctor[H[_[_, _], _, _]] {
    def hbimap[F[_, _], G[_, _]](biNt: F ~>> G): H[F, ?, ?] ~>> H[G, ?, ?]
  }

  final case class Fix[F[_[_], _], A](unFix: F[Fix[F, ?], A])
  final case class BiFix[F[_[_, _], _, _], A, B](unFix: F[BiFix[F, ?, ?], A, B])

  final case class HEnvT[E, F[_[_], _], G[_], I](ask: E, fa: F[G, I])

  object HEnvT {

    implicit def hfunctor[E, F[_[_], _]](implicit F: HFunctor[F]): HFunctor[HEnvT[E, F, ?[_], ?]] =
      new HFunctor[HEnvT[E, F, ?[_], ?]] {

        def hmap[M[_], N[_]](nt: M ~> N) = new (HEnvT[E, F, M, ?] ~> HEnvT[E, F, N, ?]) {
          def apply[I](fm: HEnvT[E, F, M, I]) = HEnvT(fm.ask, F.hmap(nt)(fm.fa))
        }
      }
  }

  final case class HBiEnvT[E, F[_[_, _], _, _], G[_, _], I, J](ask: E, fa: F[G, I, J])

  object HBiEnvT {
    implicit def hbifunctor[E, H[_[_, _], _, _]](
      implicit H: HBiFunctor[H]
    ): HBiFunctor[HBiEnvT[E, H, ?[_, _], ?, ?]] = new HBiFunctor[HBiEnvT[E, H, ?[_, _], ?, ?]] {

      def hbimap[F[_, _], G[_, _]](
        biNt: F ~>> G
      ): HBiEnvT[E, H, F, ?, ?] ~>> HBiEnvT[E, H, G, ?, ?] =
        new BiNaturalTransformation[HBiEnvT[E, H, F, ?, ?], HBiEnvT[E, H, G, ?, ?]] {

          def apply[A, B](x: HBiEnvT[E, H, F, A, B]): HBiEnvT[E, H, G, A, B] =
            HBiEnvT(x.ask, H.hbimap(biNt)(x.fa))
        }
    }
  }

  final case class HEnvTK[E[_], F[_[_], _], G[_], I](ask: E[I], fa: F[G, I])

  object HEnvTK {
    implicit def hfunctor[E[_], F[_[_], _]](
      implicit F: HFunctor[F]
    ): HFunctor[HEnvTK[E, F, ?[_], ?]] = new HFunctor[HEnvTK[E, F, ?[_], ?]] {

      def hmap[M[_], N[_]](nt: M ~> N) = new (HEnvTK[E, F, M, ?] ~> HEnvTK[E, F, N, ?]) {
        def apply[I](fm: HEnvTK[E, F, M, I]) = HEnvTK(fm.ask, F.hmap(nt)(fm.fa))
      }
    }
  }

  final case class HBiEnvTK[E[_], F[_[_, _], _, _], G[_, _], I, J](ask: E[J], fa: F[G, I, J])

  object HBiEnvTK {
    implicit def hbifunctor[E[_], H[_[_, _], _, _]](
      implicit H: HBiFunctor[H]
    ): HBiFunctor[HBiEnvTK[E, H, ?[_, _], ?, ?]] = new HBiFunctor[HBiEnvTK[E, H, ?[_, _], ?, ?]] {

      def hbimap[F[_, _], G[_, _]](
        biNt: F ~>> G
      ): HBiEnvTK[E, H, F, ?, ?] ~>> HBiEnvTK[E, H, G, ?, ?] =
        new BiNaturalTransformation[HBiEnvTK[E, H, F, ?, ?], HBiEnvTK[E, H, G, ?, ?]] {

          def apply[A, B](x: HBiEnvTK[E, H, F, A, B]): HBiEnvTK[E, H, G, A, B] =
            HBiEnvTK(x.ask, H.hbimap(biNt)(x.fa))
        }
    }
  }

}

package object recursion {

  type HAlgebra[F[_[_], _], G[_]]   = F[G, ?] ~> G
  type HCoalgebra[F[_[_], _], G[_]] = G ~> F[G, ?]

  def cataNT[S[_[_], _], F[_]](
    alg: HAlgebra[S, F]
  )(implicit S: HFunctor[S]): (Fix[S, ?] ~> F) =
    new (Fix[S, ?] ~> F) { self =>

      def apply[A](f: Fix[S, A]): F[A] =
        alg.apply[A](S.hmap(self)(f.unFix))
    }

  def hyloNT[S[_[_], _], F[_], G[_]](coalgebra: HCoalgebra[S, F], algebra: HAlgebra[S, G])(
    implicit S: HFunctor[S]
  ): F ~> G = new (F ~> G) { self =>

    def apply[A](fa: F[A]): G[A] =
      algebra(S.hmap(self)(coalgebra(fa)))
  }

  type HBiAlgebra[F[_[_, _], _, _], G[_, _]]   = F[G, ?, ?] ~>> G
  type HBiCoAlgebra[F[_[_, _], _, _], G[_, _]] = G ~>> F[G, ?, ?]

  def biCataNT[S[_[_, _], _, _], F[_, _]](alg: HBiAlgebra[S, F])(
    implicit S: HBiFunctor[S]
  ): (BiFix[S, ?, ?] ~>> F) = new BiNaturalTransformation[BiFix[S, ?, ?], F] { self =>
    def apply[A, B](f: BiFix[S, A, B]): F[A, B] = alg.apply[A, B](S.hbimap(self)(f.unFix))
  }

  def biHyloNT[S[_[_, _], _, _], F[_, _], G[_, _]](
    coalg: HBiCoAlgebra[S, F],
    alg: HBiAlgebra[S, G]
  )(implicit S: HBiFunctor[S]): F ~>> G = new BiNaturalTransformation[F, G] { self =>
    def apply[A, B](f: F[A, B]): G[A, B] = alg(S.hbimap(self)(coalg(f)))
  }

}
