package scalaz

package schema

import recursion._

import monocle.Iso

trait Realisation {
  type Prim[A]
  type SumTermId
  type ProductTermId
}

/*
trait BiNat[F[_,_],G[_,_]]{
  def apply[A,B](f:F[A,B]):G[A,B]
}
object BiNat {
  type ~>>[F[_,_],G[_,_]] = BiNat[F,G]

  def compose[F[_,_],G[_,_],H[_,_]](x: F ~>> G, y: G ~>> H): F ~>> H = new BiNat[F, H] {
    override def apply[A,B](f:F[A,B]):H[A,B] = y(x(f))
  }

  implicit class BiNatSyntax[F[_,_],G[_,_]](biNt: F ~>> G){
    def compose[H[_,_]](other: G ~>> H):F ~>> H = BiNat.compose(biNt, other)
  }
}



import BiNat._
 */

import scalaz.{ BiNaturalTransformation => ~>> }

sealed trait SchemaF[Prim[_], SumTermId, ProductTermId, F[_, _], AT, A] {
  def hmap[G[_, _]](nt: F ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, AT, A]
}

////////////////////
// The Schema ADT
////////////////////

// "Essential" nodes. In theory every possible type can be represented using only `One`, `:+:` and `:*:`

final case class One[F[_, _], Prim[_], SumTermId, ProductTermId]()
    extends SchemaF[Prim, SumTermId, ProductTermId, F, Unit, Unit] {
  def hmap[G[_, _]](nt: F ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, Unit, Unit] = One()
}

/**
 * The sum of two schemas, yielding the schema for `A \/ B`
 */
final case class SumF[F[_, _], AT, A, BT, B, Prim[_], SumTermId, ProductTermId](
  left: F[AT, A],
  right: F[BT, B]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, AT \/ BT, A \/ B] {

  def hmap[G[_, _]](nt: F ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, AT \/ BT, A \/ B] =
    SumF(nt(left), nt(right))
  override def toString: String = s"$left :+: $right"
}

/**
 * The product of two schemas, yielding the schema for `(A, B)`
 */
final case class ProdF[F[_, _], AT, A, BT, B, Prim[_], SumTermId, ProductTermId](
  left: F[AT, A],
  right: F[BT, B]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, (AT, BT), (A, B)] {

  def hmap[G[_, _]](nt: F ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, (AT, BT), (A, B)] =
    ProdF(nt(left), nt(right))
  override def toString: String = s"$left :*: $right"
}

// "Extra" nodes, making it more convenient to represent real-world types

/**
 * The schema of a primitive type in the context of this `SchemaModule`
 */
final case class PrimSchemaF[F[_, _], A, Prim[_], SumTermId, ProductTermId](prim: Prim[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, A, A] {

  def hmap[G[_, _]](nt: F ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, A, A] =
    PrimSchemaF[G, A, Prim, SumTermId, ProductTermId](prim)
}

/**
 * A named branch of an union
 */
final case class BranchF[F[_, _], AT, A, Prim[_], SumTermId, ProductTermId](
  id: SumTermId,
  schema: F[AT, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, (SumTermId, AT), A] {

  def hmap[G[_, _]](nt: F ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, (SumTermId, AT), A] =
    BranchF(id, nt(schema))
}

/**
 * An union, eg. a sum of named branches
 * This class cannot be constructed directly, you must use the `SchemaModule#union` method.
 */
sealed abstract case class UnionF[F[_, _], A, AE, Prim[_], SumTermId, ProductTermId](
  choices: F[AE, AE],
  iso: Iso[AE, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, AE, A] {

  def hmap[G[_, _]](nt: F ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, AE, A] =
    new UnionF[G, A, AE, Prim, SumTermId, ProductTermId](nt(choices), iso) {}
}

/**
 * A named field of a record
 */
final case class FieldF[F[_, _], AT, A, Prim[_], SumTermId, ProductTermId](
  id: ProductTermId,
  schema: F[AT, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, (ProductTermId, AT), A] {

  def hmap[G[_, _]](
    nt: F ~>> G
  ): SchemaF[Prim, SumTermId, ProductTermId, G, (ProductTermId, AT), A] =
    FieldF(id, nt(schema))
}

/**
 * A record, eg. a product of named fields
 * This class cannot be constructed directly, you must use the `SchemaModule#record` method.
 */
sealed abstract case class RecordF[F[_, _], A, AP, Prim[_], SumTermId, ProductTermId](
  fields: F[AP, AP],
  iso: Iso[AP, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, Iso[AP, A], A] {

  def hmap[G[_, _]](nt: F ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, Iso[AP, A], A] =
    new RecordF[G, A, AP, Prim, SumTermId, ProductTermId](nt(fields), iso) {}
}

/**
 * A sequence
 */
final case class SeqF[F[_, _], AT, A, Prim[_], SumTermId, ProductTermId](element: F[AT, A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, List[AT], List[A]] {

  def hmap[G[_, _]](nt: F ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, List[AT], List[A]] =
    SeqF(nt(element))
}

/**
 * The schema obtained by "mapping" an Iso of top of a schema. If there is an isomorphism
 * between AO and A, then a schema of A0 can be used to represent values of A.
 */
final case class IsoSchemaF[F[_, _], AT, A0, A, Prim[_], SumTermId, ProductTermId](
  base: F[AT, A0],
  iso: Iso[A0, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, Iso[AT, A], A] {

  def hmap[G[_, _]](nt: F ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, Iso[AT, A], A] =
    IsoSchemaF(nt(base), iso)
}

final case class ConstSchemaF[F[_, _], AT, A, Prim[_], SumTermId, ProductTermId](
  base: F[AT, A],
  value: A
) extends SchemaF[Prim, SumTermId, ProductTermId, F, AT, A] {

  def hmap[G[_, _]](nt: F ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, AT, A] =
    ConstSchemaF(nt(base), value)
}

final case class SelfReference[F[_, _], H[_, _], AT, A, Prim[_], SumTermId, ProductTermId](
  private val ref: () => F[AT, A],
  private val nattrans: F ~>> H
) extends SchemaF[Prim, SumTermId, ProductTermId, H, AT, A] {

  lazy val unroll: H[AT, A] = nattrans(ref())

  def hmap[G[_, _]](nt: H ~>> G): SchemaF[Prim, SumTermId, ProductTermId, G, AT, A] =
    SelfReference[F, G, AT, A, Prim, SumTermId, ProductTermId](ref, nt.compose(nattrans))
}

final case class RightOnly[F[_], A, B](fb: F[B])

final case class HProduct[F[_], A, B](fa: F[A], fb: F[B])

object HProduct {

  def fromNT[F[_], G[_]](nt: F ~> G): HProduct[F, ?, ?] ~>> HProduct[G, ?, ?] =
    new BiNaturalTransformation[HProduct[F, ?, ?], HProduct[G, ?, ?]] {
      def apply[A, B](fab: HProduct[F, A, B]): HProduct[G, A, B] = HProduct(nt(fab.fa), nt(fab.fb))
    }
}

trait BiInterpreter[F[_, _], G[_, _]] { self =>

  def interpret: F ~>> G

  def compose[H[_, _]](biNt: H ~>> F) = self match {
    case i: ComposedBiInterpreter[h, G, F] =>
      ComposedBiInterpreter(i.underlying, i.biNt.compose(biNt))
    case x => ComposedBiInterpreter(x, biNt)
  }
}

final case class ComposedBiInterpreter[F[_, _], G[_, _], H[_, _]](
  underlying: BiInterpreter[F, G],
  biNt: H ~>> F
) extends BiInterpreter[H, G] {
  final override val interpret = underlying.interpret.compose(biNt)
}

final case class BiCataInterpreter[S[_[_, _], _, _], F[_, _]](alg: HBiAlgebra[S, F])(
  implicit ev: HBiFunctor[S]
) extends BiInterpreter[BiFix[S, ?, ?], F] {
  final override val interpret = biCataNT(alg)
}

final case class BiHyloInterpreter[S[_[_, _], _, _], F[_, _], G[_, _]](
  coalg: HBiCoAlgebra[S, G],
  alg: HBiAlgebra[S, F]
)(implicit ev: HBiFunctor[S])
    extends BiInterpreter[G, F] {
  final override val interpret = biHyloNT(coalg, alg)
}

/**
 * An interpreter able to derive a `F[A]` from a schema for `A` (for any `A`).
 * Such interpreters will usually be implemented using a recursion scheme like
 * 'cataNT`or hyloNT`.
 */
trait Interpreter[F[_], G[_]] { self =>

  /**
   * A natural transformation that will transform a schema for any type `A`
   * into an `F[A]`.
   */
  def interpret: F ~> G

  def compose[H[_]](nt: H ~> F) = self match {
    case i: ComposedInterpreter[h, G, F] => ComposedInterpreter(i.underlying, i.nt.compose(nt))
    case x                               => ComposedInterpreter(x, nt)
  }
}

final case class ComposedInterpreter[F[_], G[_], H[_]](underlying: Interpreter[F, G], nt: H ~> F)
    extends Interpreter[H, G] {
  final override val interpret = underlying.interpret.compose(nt)
}

class CataInterpreter[S[_[_], _], F[_]](
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[Fix[S, ?], F] {
  final override val interpret = cataNT(algebra)
}

class HyloInterpreter[S[_[_], _], F[_], G[_]](
  coalgebra: HCoalgebra[S, G],
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[G, F] {
  final override val interpret = hyloNT(coalgebra, algebra)
}

object SchemaF {

  implicit def schemaHBiFunctor[Prim[_], SumTermId, ProductTermId] =
    new HBiFunctor[SchemaF[Prim, SumTermId, ProductTermId, ?[_, _], ?, ?]] {

      def hbimap[F[_, _], G[_, _]](nt: F ~>> G) =
        new (SchemaF[Prim, SumTermId, ProductTermId, F, ?, ?] ~>> SchemaF[
          Prim,
          SumTermId,
          ProductTermId,
          G,
          ?,
          ?
        ]) {
          def apply[AT, A](fa: SchemaF[Prim, SumTermId, ProductTermId, F, AT, A]) = fa.hmap(nt)
        }
    }

  type FSchema[Prim[_], SumTermId, ProductTermId, AT, A] =
    BiFix[SchemaF[Prim, SumTermId, ProductTermId, ?[_, _], ?, ?], AT, A]

  sealed private[schema] trait LabelledSum_[AT, A, Prim[_], SumTermId, ProductTermId] {
    def toSchema: FSchema[Prim, SumTermId, ProductTermId, AT, A]

    def :+: [BT, B](
      l: LabelledSum_[BT, B, Prim, SumTermId, ProductTermId]
    ): LabelledSum_[BT \/ AT, B \/ A, Prim, SumTermId, ProductTermId] = LabelledSum2(l, this)
  }

  final private[schema] case class LabelledSum1[AT, A, Prim[_], SumTermId, ProductTermId](
    id: SumTermId,
    schema: FSchema[Prim, SumTermId, ProductTermId, AT, A]
  ) extends LabelledSum_[(SumTermId, AT), A, Prim, SumTermId, ProductTermId] {
    def toSchema = BiFix(BranchF(id, schema))

  }

  final private[schema] case class LabelledSum2[AT, A, BT, B, Prim[_], SumTermId, ProductTermId](
    l: LabelledSum_[AT, A, Prim, SumTermId, ProductTermId],
    r: LabelledSum_[BT, B, Prim, SumTermId, ProductTermId]
  ) extends LabelledSum_[AT \/ BT, A \/ B, Prim, SumTermId, ProductTermId] {
    def toSchema = BiFix(new SumF(l.toSchema, r.toSchema))

  }

  sealed private[schema] trait LabelledProduct_[AT, A, Prim[_], SumTermId, ProductTermId] {
    def toSchema: FSchema[Prim, SumTermId, ProductTermId, AT, A]

    def :*: [BT, B](
      l: LabelledProduct_[BT, B, Prim, SumTermId, ProductTermId]
    ): LabelledProduct_[(BT, AT), (B, A), Prim, SumTermId, ProductTermId] =
      LabelledProduct2(l, this)
  }

  final private[schema] case class LabelledProduct1[AT, A, Prim[_], SumTermId, ProductTermId](
    id: ProductTermId,
    schema: FSchema[Prim, SumTermId, ProductTermId, AT, A]
  ) extends LabelledProduct_[(ProductTermId, AT), A, Prim, SumTermId, ProductTermId] {
    def toSchema = BiFix(FieldF(id, schema))

  }

  final private[schema] case class LabelledProduct2[AT, A, BT, B, Prim[_], SumTermId, ProductTermId](
    l: LabelledProduct_[AT, A, Prim, SumTermId, ProductTermId],
    r: LabelledProduct_[BT, B, Prim, SumTermId, ProductTermId]
  ) extends LabelledProduct_[(AT, BT), (A, B), Prim, SumTermId, ProductTermId] {
    def toSchema = BiFix(new ProdF(l.toSchema, r.toSchema))

  }
}

trait SchemaModule[R <: Realisation] {

  val R: R

  import SchemaF._

  type RInterpreter[F[_, _]] = BiInterpreter[Schema, F]

  type RSchema[F[_, _], AT, A] = SchemaF[R.Prim, R.SumTermId, R.ProductTermId, F, AT, A]

  type Schema[AT, A] = FSchema[R.Prim, R.SumTermId, R.ProductTermId, AT, A]

  type LabelledSum[AT, A] = LabelledSum_[AT, A, R.Prim, R.SumTermId, R.ProductTermId]

  type LabelledProduct[AT, A] = LabelledProduct_[AT, A, R.Prim, R.SumTermId, R.ProductTermId]

  type ROne[F[_, _]]                = One[F, R.Prim, R.SumTermId, R.ProductTermId]
  type Sum[F[_, _], AT, A, BT, B]   = SumF[F, AT, A, BT, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Prod[F[_, _], AT, A, BT, B]  = ProdF[F, AT, A, BT, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Branch[F[_, _], AT, A]       = BranchF[F, AT, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Union[F[_, _], AE, A]        = UnionF[F, A, AE, R.Prim, R.SumTermId, R.ProductTermId]
  type Field[F[_, _], AT, A]        = FieldF[F, AT, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Record[F[_, _], An, A]       = RecordF[F, A, An, R.Prim, R.SumTermId, R.ProductTermId]
  type Sequence[F[_, _], AT, A]     = SeqF[F, AT, A, R.Prim, R.SumTermId, R.ProductTermId]
  type IsoSchema[F[_, _], AT, A, B] = IsoSchemaF[F, AT, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type ConstSchema[F[_, _], AT, A]  = ConstSchemaF[F, AT, A, R.Prim, R.SumTermId, R.ProductTermId]
  type PrimSchema[F[_, _], A]       = PrimSchemaF[F, A, R.Prim, R.SumTermId, R.ProductTermId]

  object BiInterpreter {

    def cata[S[_[_, _], _, _], F[_, _]](alg: HBiAlgebra[S, F])(implicit ev: HBiFunctor[S]) =
      new BiCataInterpreter[S, F](alg)

    def hylo[S[_[_, _], _, _], F[_, _], G[_, _]](coalg: HBiCoAlgebra[S, G], alg: HBiAlgebra[S, F])(
      implicit ev: HBiFunctor[S]
    ) = new BiHyloInterpreter(coalg, alg)
  }

  object Interpreter {

    def cata[S[_[_], _], F[_]](alg: HAlgebra[S, F])(implicit ev: HFunctor[S]) =
      new CataInterpreter[S, F](alg)

    def hylo[S[_[_], _], F[_], G[_]](coalg: HCoalgebra[S, G], alg: HAlgebra[S, F])(
      implicit ev: HFunctor[S]
    ) = new HyloInterpreter(coalg, alg)

  }

  ////////////////
  // Public API
  ////////////////

  implicit final class SchemaSyntax[AT, A](schema: Schema[AT, A]) {

    def :*: [BT, B](left: Schema[BT, B]): Schema[(BT, AT), (B, A)] = BiFix(new ProdF(left, schema))

    def :+: [BT, B](left: Schema[BT, B]): Schema[BT \/ AT, B \/ A] = BiFix(new SumF(left, schema))

    def -*>: (id: R.ProductTermId): LabelledProduct[(R.ProductTermId, AT), A] =
      LabelledProduct1(id, schema)

    def -+>: (id: R.SumTermId): LabelledSum[(R.SumTermId, AT), A] = LabelledSum1(id, schema)

    def to[F[_, _]](implicit interpreter: RInterpreter[F]): F[AT, A] = interpreter.interpret(schema)

    def imap[B](_iso: Iso[A, B]): Schema[Iso[AT, B], B] = (schema.unFix) match {
      case is: IsoSchemaF[
            FSchema[R.Prim @unchecked, R.SumTermId @unchecked, R.ProductTermId @unchecked, ?, ?] @unchecked,
            AT @unchecked,
            _,
            A @unchecked,
            R.Prim @unchecked,
            R.SumTermId @unchecked,
            R.ProductTermId @unchecked
          ] =>
        iso(is.base, is.iso.composeIso(_iso))
      case _ => BiFix(IsoSchemaF(schema, _iso))
    }

  }

  final def unit: Schema[Unit, Unit] =
    BiFix(
      One[
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?, ?],
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ]()
    )

  final def prim[A](prim: R.Prim[A]): Schema[A, A] =
    BiFix(PrimSchemaF(prim))

  final def union[A, AE](choices: LabelledSum[AE, AE], iso: Iso[AE, A]): Schema[AE, A] =
    BiFix(
      new UnionF[
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?, ?],
        A,
        AE,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](choices.toSchema, iso) {}
    )

  final def optional[AT, A](aSchema: Schema[AT, A]): Schema[Iso[AT \/ Unit, Option[A]], Option[A]] =
    iso[AT \/ Unit, A \/ Unit, Option[A]](
      BiFix(
        new SumF[
          FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?, ?],
          AT,
          A,
          Unit,
          Unit,
          R.Prim,
          R.SumTermId,
          R.ProductTermId
        ](aSchema, unit)
      ),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  final def record[A, An](terms: LabelledProduct[An, An], isoA: Iso[An, A]): Schema[Iso[An, A], A] =
    BiFix(
      new RecordF[
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?, ?],
        A,
        An,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](terms.toSchema, isoA) {}
    )

  final def seq[AT, A](element: Schema[AT, A]): Schema[List[AT], List[A]] =
    BiFix(
      SeqF[
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?, ?],
        AT,
        A,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](element)
    )

  final def iso[AT, A0, A](base: Schema[AT, A0], iso: Iso[A0, A]): Schema[Iso[AT, A], A] =
    BiFix(IsoSchemaF(base, iso))

  final def self[AT, A](root: => Schema[AT, A]): Schema[AT, A] =
    BiFix(
      SelfReference(
        () => root,
        new BiNaturalTransformation[Schema, Schema] {
          def apply[X, Y](a: Schema[X, Y]): Schema[X, Y] = a
        }
      )
    )
}
