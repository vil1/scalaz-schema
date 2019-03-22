package scalaz

package schema

import recursion._

import SchemaF._

//import monocle.Iso
import scalaz.{ BiNaturalTransformation => ~>> }

sealed trait MigrationStep[Prim[_], SumTermId, ProductTermId, G[_[_, _], _, _]] {
  type SchemaH[F[_, _], AT, A] = SchemaF[Prim, SumTermId, ProductTermId, F, AT, A]

  type HSchema[AT, A] = FSchema[
    Prim,
    SumTermId,
    ProductTermId,
    AT,
    A
  ]

  //private def addField[A, B](base: Schema[B], default: A) = Iso[B, (A, B)](b => (default, b))(_._2)

  def algebra: HBiAlgebra[G, HSchema]

  def coalgebra: HBiCoAlgebra[G, HSchema]
}

object MigrationStep {}

final case class AddField[A, Prim[_], SumTermId, ProductTermId](
  name: ProductTermId,
//schema: FSchema[Prim, SumTermId, ProductTermId, A],
  default: A
) extends MigrationStep[
      Prim,
      SumTermId,
      ProductTermId,
      HBiEnvTK[Option, SchemaF[Prim, SumTermId, ProductTermId, ?[_, _], ?, ?], ?[_, _], ?, ?]
    ] {
  type HEnvSchema[F[_, _], X, Y] = HBiEnvTK[Option, SchemaH, F, X, Y]

  override val algebra = new (HEnvSchema[HSchema, ?, ?] ~>> HSchema) {

    def apply[X, Y](env: HEnvSchema[HSchema, X, Y]): HSchema[X, Y] =
      env.ask.fold[HSchema[X, Y]](BiFix(env.fa))(y => BiFix(ConstSchemaF(BiFix(env.fa), y)))
  }
  override val coalgebra = new (HSchema ~>> HEnvSchema[HSchema, ?, ?]) {

    def apply[X, Y](schema: HSchema[X, Y]): HEnvSchema[HSchema, X, Y] = schema.unFix match {
      //How do we proof that default actually has the correct type here? right now we don't at all and default could be whatever
      case x: FieldF[HSchema, X, Y, Prim, SumTermId, ProductTermId] =>
        if (x.id == name) HBiEnvTK(Some(default.asInstanceOf[Y]), x) else HBiEnvTK(None, x)
      case x => HBiEnvTK(None, x)
    }
  }
}

trait HasMigrations[R <: Realisation] extends SchemaModule[R] {

  //import Scalaz._
  ///import R._

  final class UpgradingSchema[G[_[_, _], _, _]: HBiFunctor](
    step: MigrationStep[R.Prim, R.SumTermId, R.ProductTermId, G]
  ) {
    type Err = String

    def to[AT, A](current: Schema[AT, A]): Schema[AT, A] =
      biHyloNT(step.coalgebra, step.algebra).apply(current)
  }

  object Schema {

    def upgradingVia[G[_[_, _], _, _]: HBiFunctor](
      step: MigrationStep[R.Prim, R.SumTermId, R.ProductTermId, G]
    ): UpgradingSchema[G] =
      new UpgradingSchema(step)
  }

}
