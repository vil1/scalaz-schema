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

  type HEnvSchema[F[_], A] = HEnvTK[Option, SchemaH, F, A]

  def algebra: HAlgebra[HEnvSchema, HSchema] = new (HEnvSchema[HSchema, ?] ~> HSchema) {

    def apply[A](env: HEnvSchema[HSchema, A]): HSchema[A] =
      env.ask.fold[HSchema[A]](Fix(env.fa))(a => Fix(ConstSchemaF(a)))
  }

  def coalgebra: HCoalgebra[HEnvSchema, HSchema] = new (HSchema ~> HEnvSchema[HSchema, ?]) {
    def apply[A](schema: HSchema[A]): HEnvSchema[HSchema, A] = ???
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
