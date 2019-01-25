package scalaz

package schema

import monocle.Iso

final case class Fix[F[_[_], _], A](unFix: F[Fix[F, ?], A])

trait Realisation {
  type Prim[A]
  type SumTermId
  type ProductTermId
}

sealed trait Schema[Prim[_], SumTermId, ProductTermId, F[_], A] { //self =>
  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A]
  //    def imap[B](iso: Iso[A, B]): Schema.FSchema[B] = Schema.IsoSchema(self, iso)
}

////////////////////
// The Schema ADT
////////////////////

// "Essential" nodes. In theory every possible type can be represented using only `One`, `:+:` and `:*:`

final case class One[Prim[_], SumTermId, ProductTermId, F[_]]()
    extends Schema[Prim, SumTermId, ProductTermId, F, Unit] {
  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, Unit] = One()
}

/**
 * The sum of two schemas, yielding the schema for `A \/ B`
 */
final case class :+:[Prim[_], SumTermId, ProductTermId, F[_], A, B](left: F[A], right: F[B])
    extends Schema[Prim, SumTermId, ProductTermId, F, A \/ B] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A \/ B] =
    :+:(nt(left), nt(right))
  override def toString: String = s"$left :+: $right"
}

/**
 * The product of two schemas, yielding the schema for `(A, B)`
 */
final case class :*:[F[_], A, B, Prim[_], SumTermId, ProductTermId](left: F[A], right: F[B])
    extends Schema[Prim, SumTermId, ProductTermId, F, (A, B)] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, (A, B)] =
    :*:(nt(left), nt(right))
  override def toString: String = s"$left :*: $right"
}

// "Extra" nodes, making it more convenient to represent real-world types

/**
 * The schema of a primitive type in the context of this `SchemaModule`
 */
final case class PrimSchema[F[_], A, Prim[_], SumTermId, ProductTermId](prim: Prim[A])
    extends Schema[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] =
    PrimSchema[G, A, Prim, SumTermId, ProductTermId](prim)
}

/**
 * A named branch of an union
 */
final case class SumTerm[F[_], A, Prim[_], SumTermId, ProductTermId](id: SumTermId, schema: F[A])
    extends Schema[Prim, SumTermId, ProductTermId, F, A] {
  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] = SumTerm(id, nt(schema))
}

/**
 * An union, eg. a sum of named branches
 * This class cannot be constructed directly, you must use the `SchemaModule#union` method.
 */
sealed abstract case class Union[Prim[_], SumTermId, ProductTermId, F[_], A, AE](
  choices: F[AE],
  iso: Iso[AE, A]
) extends Schema[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] =
    new Union[Prim, SumTermId, ProductTermId, G, A, AE](nt(choices), iso) {}
}

/**
 * A named field of a record
 */
final case class ProductTerm[F[_], A, Prim[_], SumTermId, ProductTermId](
  id: ProductTermId,
  schema: F[A]
) extends Schema[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] =
    ProductTerm(id, nt(schema))
}

/**
 * A record, eg. a product of named fields
 * This class cannot be constructed directly, you must use the `SchemaModule#record` method.
 */
sealed abstract case class Record[Prim[_], SumTermId, ProductTermId, F[_], A, AP](
  fields: F[AP],
  iso: Iso[AP, A]
) extends Schema[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] =
    new Record[Prim, SumTermId, ProductTermId, G, A, AP](nt(fields), iso) {}
}

/**
 * A sequence
 */
final case class SeqSchema[F[_], A, Prim[_], SumTermId, ProductTermId](element: F[A])
    extends Schema[Prim, SumTermId, ProductTermId, F, List[A]] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, List[A]] =
    SeqSchema(nt(element))
}

/**
 * The schema obtained by "mapping" an Iso of top of a schema. If there is an isomorphism
 * between AO and A, then a schema of A0 can be used to represent values of A.
 */
final case class IsoSchema[Prim[_], SumTermId, ProductTermId, F[_], A0, A](
  base: F[A0],
  iso: Iso[A0, A]
) extends Schema[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] =
    IsoSchema(nt(base), iso)
}

object Schema {

  import RecursionSchemes._

  type FSchema[Prim[_], SumTermId, ProductTermId, A] =
    Fix[Schema[Prim, SumTermId, ProductTermId, ?[_], ?], A]

  sealed private[schema] trait LabelledSum[Prim[_], SumTermId, ProductTermId, A] {
    def toSchema: FSchema[Prim, SumTermId, ProductTermId, A]

    def :+: [B](
      l: LabelledSum[Prim, SumTermId, ProductTermId, B]
    ): LabelledSum[Prim, SumTermId, ProductTermId, B \/ A] = LabelledSum2(l, this)
  }

  final private[schema] case class LabelledSum1[Prim[_], SumTermId, ProductTermId, A](
    id: SumTermId,
    schema: FSchema[Prim, SumTermId, ProductTermId, A]
  ) extends LabelledSum[Prim, SumTermId, ProductTermId, A] {
    def toSchema = Fix(SumTerm(id, schema))

  }

  final private[schema] case class LabelledSum2[Prim[_], SumTermId, ProductTermId, A, B](
    l: LabelledSum[Prim, SumTermId, ProductTermId, A],
    r: LabelledSum[Prim, SumTermId, ProductTermId, B]
  ) extends LabelledSum[Prim, SumTermId, ProductTermId, A \/ B] {
    def toSchema = Fix(new :+:(l.toSchema, r.toSchema))

  }

  sealed private[schema] trait LabelledProduct[Prim[_], SumTermId, ProductTermId, A] {
    def toSchema: FSchema[Prim, SumTermId, ProductTermId, A]

    def :*: [B](
      l: LabelledProduct[Prim, SumTermId, ProductTermId, B]
    ): LabelledProduct[Prim, SumTermId, ProductTermId, (B, A)] = LabelledProduct2(l, this)
  }

  final private[schema] case class LabelledProduct1[Prim[_], SumTermId, ProductTermId, A](
    id: ProductTermId,
    schema: FSchema[Prim, SumTermId, ProductTermId, A]
  ) extends LabelledProduct[Prim, SumTermId, ProductTermId, A] {
    def toSchema = Fix(ProductTerm(id, schema))

  }

  final private[schema] case class LabelledProduct2[Prim[_], SumTermId, ProductTermId, A, B](
    l: LabelledProduct[Prim, SumTermId, ProductTermId, A],
    r: LabelledProduct[Prim, SumTermId, ProductTermId, B]
  ) extends LabelledProduct[Prim, SumTermId, ProductTermId, (A, B)] {
    def toSchema = Fix(new :*:(l.toSchema, r.toSchema))

  }

  // Schema syntax

  implicit final class SchemaSyntax[A, Prim[_], SumTermId, ProductTermId](
    schema: FSchema[Prim, SumTermId, ProductTermId, A]
  ) {

    def :*: [B](
      left: FSchema[Prim, SumTermId, ProductTermId, B]
    ): FSchema[Prim, SumTermId, ProductTermId, (B, A)] = Fix(new :*:(left, schema))

    def :+: [B](
      left: FSchema[Prim, SumTermId, ProductTermId, B]
    ): FSchema[Prim, SumTermId, ProductTermId, B \/ A] = Fix(new :+:(left, schema))

    def -*>: (id: ProductTermId): LabelledProduct[Prim, SumTermId, ProductTermId, A] =
      LabelledProduct1(id, schema)

    def -+>: (id: SumTermId): LabelledSum[Prim, SumTermId, ProductTermId, A] =
      LabelledSum1(id, schema)

    def to[F[_]](
      implicit algebra: HAlgebra[Schema[Prim, SumTermId, ProductTermId, ?[_], ?], F],
      functor: HFunctor[Schema[Prim, SumTermId, ProductTermId, ?[_], ?]]
    ): F[A] = cata(algebra).apply(schema)

    def imap[B](_iso: Iso[A, B]): FSchema[Prim, SumTermId, ProductTermId, B] = schema.unFix match {
      case IsoSchema(base, iso) => Fix(IsoSchema(base, iso.composeIso(_iso)))
      case _                    => Fix(IsoSchema(schema, _iso))
    }

  }

  ///////////////////////
  // Schema operations
  ///////////////////////
}

trait SchemaModule[R <: Realisation] {

  val R: R

  import Schema._
  import RecursionSchemes._

  type RProductTerm[F[_], A] = ProductTerm[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type RSumTerm[F[_], A]     = SumTerm[F, A, R.Prim, R.SumTermId, R.ProductTermId]

  type RSchema[F[_], A] = Schema[R.Prim, R.SumTermId, R.ProductTermId, F, A]

  implicit object schemaHFunctor extends HTraverse[RSchema] {

    override def hmap[F[_], G[_]](f: F ~> G) =
      new (RSchema[F, ?] ~> RSchema[G, ?]) {

        def apply[X](
          fax: RSchema[F, X]
        ): RSchema[G, X] = fax.hmap(f)
      }

    override def sequence[G[_], A[_]](implicit G: Applicative[G]) =
      new (RSchema[λ[α => G[A[α]]], ?] ~> λ[α => G[RSchema[A, α]]]) {
        type GA[X] = G[A[X]]

        def apply[X](sgax: RSchema[GA, X]): G[RSchema[A, X]] = sgax match {
          case _: One[R.Prim, R.SumTermId, R.ProductTermId, GA] =>
            G.point(One[R.Prim, R.SumTermId, R.ProductTermId, A]())
          case s: :+:[R.Prim, R.SumTermId, R.ProductTermId, GA, a, b] =>
            G.apply2(s.left, s.right)((l: A[a], r: A[b]) => :+:(l, r))
          case p: :*:[GA, a, b, R.Prim, R.SumTermId, R.ProductTermId] =>
            G.apply2(p.left, p.right)((l: A[a], r: A[b]) => :*:(l, r))
          case p: PrimSchema[GA, a, R.Prim, R.SumTermId, R.ProductTermId] =>
            G.point(PrimSchema[A, a, R.Prim, R.SumTermId, R.ProductTermId](p.prim))
          case t: SumTerm[GA, a, R.Prim, R.SumTermId, R.ProductTermId] =>
            G.map(t.schema)(
              (s: A[a]) => SumTerm[A, a, R.Prim, R.SumTermId, R.ProductTermId](t.id, s)
            )
          case u: Union[R.Prim, R.SumTermId, R.ProductTermId, GA, a, ae] =>
            G.map(u.choices)(
              (s: A[ae]) => new Union[R.Prim, R.SumTermId, R.ProductTermId, A, a, ae](s, u.iso) {}
            )
          case t: ProductTerm[GA, a, R.Prim, R.SumTermId, R.ProductTermId] =>
            G.map(t.schema)(
              (s: A[a]) => ProductTerm[A, a, R.Prim, R.SumTermId, R.ProductTermId](t.id, s)
            )

          case u: Record[R.Prim, R.SumTermId, R.ProductTermId, GA, a, ae] =>
            G.map(u.fields)(
              (s: A[ae]) => new Record[R.Prim, R.SumTermId, R.ProductTermId, A, a, ae](s, u.iso) {}
            )
          case s: SeqSchema[GA, a, R.Prim, R.SumTermId, R.ProductTermId] =>
            G.map(s.element)(
              (a: A[a]) => SeqSchema[A, a, R.Prim, R.SumTermId, R.ProductTermId](a)
            )
          case i: IsoSchema[R.Prim, R.SumTermId, R.ProductTermId, GA, a0, a] =>
            G.map(i.base)(
              (a: A[a0]) => IsoSchema[R.Prim, R.SumTermId, R.ProductTermId, A, a0, a](a, i.iso)
            )
        }
      }
  }

  ////////////////
  // Public API
  ////////////////

  final def unit: FSchema[R.Prim, R.SumTermId, R.ProductTermId, Unit] =
    Fix(
      One[R.Prim, R.SumTermId, R.ProductTermId, FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?]]()
    )

  final def prim[A](prim: R.Prim[A]): FSchema[R.Prim, R.SumTermId, R.ProductTermId, A] =
    Fix(PrimSchema(prim))

  final def union[A, AE](
    choices: LabelledSum[R.Prim, R.SumTermId, R.ProductTermId, AE],
    iso: Iso[AE, A]
  ): FSchema[R.Prim, R.SumTermId, R.ProductTermId, A] =
    Fix(
      new Union[
        R.Prim,
        R.SumTermId,
        R.ProductTermId,
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
        A,
        AE
      ](choices.toSchema, iso) {}
    )

  final def optional[A](
    aSchema: FSchema[R.Prim, R.SumTermId, R.ProductTermId, A]
  ): FSchema[R.Prim, R.SumTermId, R.ProductTermId, Option[A]] =
    iso(
      Fix(
        new :+:[
          R.Prim,
          R.SumTermId,
          R.ProductTermId,
          FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
          A,
          Unit
        ](aSchema, unit)
      ),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  final def record[A, An](
    terms: LabelledProduct[R.Prim, R.SumTermId, R.ProductTermId, An],
    isoA: Iso[An, A]
  ): FSchema[R.Prim, R.SumTermId, R.ProductTermId, A] =
    Fix(
      new Record[
        R.Prim,
        R.SumTermId,
        R.ProductTermId,
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
        A,
        An
      ](terms.toSchema, isoA) {}
    )

  final def seq[A](
    element: FSchema[R.Prim, R.SumTermId, R.ProductTermId, A]
  ): FSchema[R.Prim, R.SumTermId, R.ProductTermId, List[A]] =
    Fix(SeqSchema(element))

  final def iso[A0, A](
    base: FSchema[R.Prim, R.SumTermId, R.ProductTermId, A0],
    iso: Iso[A0, A]
  ): FSchema[R.Prim, R.SumTermId, R.ProductTermId, A] =
    Fix(IsoSchema(base, iso))

}
