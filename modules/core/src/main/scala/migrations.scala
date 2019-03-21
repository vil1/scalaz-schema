package scalaz

package schema

import recursion._

import SchemaF._

import monocle.Iso

sealed trait MigrationStep[Prim[_], SumTermId, ProductTermId] {

  //private def addField[A, B](base: Schema[B], default: A) = Iso[B, (A, B)](b => (default, b))(_._2)

  type SchemaH[F[_], A] = SchemaF[Prim, SumTermId, ProductTermId, F, A]

  type HSchema[A] = FSchema[
    Prim,
    SumTermId,
    ProductTermId,
    A
  ]

  type HEnvSchema[A] = HEnvT[Unit , SchemaH, HSchema, A]

  def algebra: HAlgebra[SchemaH, HEnvSchema] = new (SchemaH[HEnvSchema, ?] ~> HEnvSchema) {

    def apply[A](schema: SchemaH[HSchema, A]): HEnvSchema[A] = schema match {
      case x: PrimSchemaF[HSchema, a, Prim, SumTermId, ProductTermId]            => HEnvT((), x)
      case x: SumF[HSchema, a, b, Prim, SumTermId, ProductTermId]                => HEnvT((), x)
      case x: ProdF[HSchema, a, b, Prim, SumTermId, ProductTermId]               => HEnvT((), x)
      case x: IsoSchemaF[HSchema, a0, a, Prim, SumTermId, ProductTermId]         => HEnvT((), x)
      case x: RecordF[HSchema, a0, a, Prim, SumTermId, ProductTermId]            => HEnvT((), x)
      case x: SeqF[HSchema, a, Prim, SumTermId, ProductTermId]                   => HEnvT((), x)
      case x: FieldF[HSchema, a, Prim, SumTermId, ProductTermId]                 => HEnvT((), x)
      case x: UnionF[HSchema, a0, a, Prim, SumTermId, ProductTermId]             => HEnvT((), x)
      case x: BranchF[HSchema, a, Prim, SumTermId, ProductTermId]                => HEnvT((), x)
      case x: One[HSchema, Prim, SumTermId, ProductTermId]                       => HEnvT((), x)
      case x: SelfReference[HSchema, HSchema, a, Prim, SumTermId, ProductTermId] => HEnvT((), x)
    }
  }

  def coalgebra: HCoalgebra[SchemaH, HSchema] = new (HSchema ~> SchemaH[HSchema, ?]) {
    def apply[A](schema: HSchema[A]): SchemaH[HSchema, A] = ???
  }
}

final case class AddField[A, Prim[_], SumTermId, ProductTermId](
  name: ProductTermId,
  schema: FSchema[Prim, SumTermId, ProductTermId, A],
  default: A
) extends MigrationStep[Prim, SumTermId, ProductTermId] {

  override val algebra   = ???
  override val coalgebra = ???
}

trait HasMigrations[R <: Realisation] extends SchemaModule[R] {

  import Scalaz._
  import R._

  final class UpgradingSchema(step: MigrationStep[R.Prim, R.SumTermId, R.ProductTermId]) {
    type Err = String

    def to[A](current: Schema[A]): Schema[A] = hyloNT(step.coalgebra, step.algebra).apply(current)
  }

  object Schema {

    def upgradingVia(step: MigrationStep[R.Prim, R.SumTermId, R.ProductTermId]): UpgradingSchema =
      new UpgradingSchema(step)
  }

}
