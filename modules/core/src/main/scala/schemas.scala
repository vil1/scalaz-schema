package scalaz

package schema

object ScalaSchema extends Realisation {
  type Prim[A]       = ScalaPrim[A]
  type ProductTermId = String
  type SumTermId     = String

  sealed trait ScalaPrim[A]

  final case object ScalaShort      extends ScalaPrim[Short]
  final case object ScalaInt        extends ScalaPrim[Int]
  final case object ScalaLong       extends ScalaPrim[Long]
  final case object ScalaFloat      extends ScalaPrim[Float]
  final case object ScalaDouble     extends ScalaPrim[Double]
  final case object ScalaBigDecimal extends ScalaPrim[BigDecimal]
  final case object ScalaByte       extends ScalaPrim[Byte]
  final case object ScalaString     extends ScalaPrim[String]
  final case object ScalaChar       extends ScalaPrim[Char]
  final case object ScalaBoolean    extends ScalaPrim[Boolean]
  final case object ScalaTemporal   extends ScalaPrim[java.time.temporal.Temporal]
  final case object ScalaUnit       extends ScalaPrim[Unit]
}

trait JsonSchema extends Realisation {
  type Prim[A]       = JsonPrim[A]
  type ProductTermId = String
  type SumTermId     = String

  sealed trait JsonPrim[A]
  final case object JsonString extends JsonPrim[String]
  final case object JsonNumber extends JsonPrim[BigDecimal]
  final case object JsonBool   extends JsonPrim[Boolean]
  final case object JsonNull   extends JsonPrim[Unit]
}

object JsonSchema extends JsonSchema
