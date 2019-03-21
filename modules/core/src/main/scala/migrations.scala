package scalaz

package schema

import recursion._

import SchemaF._

import monocle.Iso

sealed trait MigrationStep[Prim[_], SumTermId, ProductTermId, G[_[_], _]] {
  type SchemaH[F[_], A] = SchemaF[Prim, SumTermId, ProductTermId, F, A]

  type HSchema[A] = FSchema[
    Prim,
    SumTermId,
    ProductTermId,
    A
  ]



  //private def addField[A, B](base: Schema[B], default: A) = Iso[B, (A, B)](b => (default, b))(_._2)



  def algebra: HAlgebra[G, HSchema] 

  def coalgebra: HCoalgebra[G, HSchema]
}

object MigrationStep{

}

final case class AddField[A, Prim[_], SumTermId, ProductTermId](
  name: ProductTermId,
//schema: FSchema[Prim, SumTermId, ProductTermId, A],
  default: A
) extends MigrationStep[Prim, SumTermId, ProductTermId, HEnvTK[Option , SchemaF[Prim, SumTermId, ProductTermId, ?[_], ?], ?[_], ?]] {
  type HEnvSchema[F[_], A] = HEnvTK[Option , SchemaH, F, A]

  override val algebra   = new (HEnvSchema[HSchema, ?] ~> HSchema) {
    def apply[X](env: HEnvSchema[HSchema, X]): HSchema[X] = env.ask.fold[HSchema[X]](Fix(env.fa))(x => Fix(ConstSchemaF(x)))
  }
  override val coalgebra  = new (HSchema ~> HEnvSchema[HSchema, ?]) {
    def apply[X](schema: HSchema[X]): HEnvSchema[HSchema, X] = schema.unFix match {
      //How do we proof that default actually has the correct type here? right now we don't at all and default could be whatever
      case x:FieldF[HSchema, X, Prim, SumTermId, ProductTermId] => if(x.id == name) HEnvTK(Some(default), x) else HEnvTK(None, x)
      case x => HEnvTK(None, x)
    }
  }
}

trait HasMigrations[R <: Realisation] extends SchemaModule[R] {

  import Scalaz._
  import R._

  final class UpgradingSchema[G[_[_], _]:HFunctor](step: MigrationStep[R.Prim, R.SumTermId, R.ProductTermId,G]) {
    type Err = String

    def to[A](current: Schema[A]): Schema[A] = hyloNT(step.coalgebra, step.algebra).apply(current)
  }

  object Schema {

    def upgradingVia[G[_[_], _]:HFunctor](step: MigrationStep[R.Prim, R.SumTermId, R.ProductTermId,G]): UpgradingSchema[G] =
      new UpgradingSchema(step)
  }

}
