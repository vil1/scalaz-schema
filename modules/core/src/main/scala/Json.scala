package scalaz

package schema

import scalaz.{ BiNaturalTransformation => ~>> }
import Liskov._

object Json {
  type JSON = String

  type Encoder[A] = A => JSON

}

trait JsonModule[R <: Realisation] extends SchemaModule[R] {
  import Json._

  implicit final def encoderInterpreter(
    implicit primNT: R.Prim ~> Encoder,
    fieldLabel: R.ProductTermId <~< String,
    branchLabel: R.SumTermId <~< String
  ): RInterpreter[RightOnly[Encoder, ?, ?]] =
    BiInterpreter.cata[RSchema, RightOnly[Encoder, ?, ?]](
      new (RSchema[RightOnly[Encoder, ?, ?], ?, ?] ~>> RightOnly[Encoder, ?, ?]) {

        val encloseInBraces         = (s: String) => s"{$s}"
        def makeField(name: String) = (s: String) => s""""$name":$s"""

        def apply[AT, A](
          schema: RSchema[RightOnly[Encoder, ?, ?], AT, A]
        ): RightOnly[Encoder, AT, A] =
          schema match {

            //case PrimSchemaF(prim)  => RightOnly(primNT(prim))
            case x: PrimSchema[RightOnly[Encoder, ?, ?], _] => RightOnly(primNT(x.prim))
            case x: Prod[RightOnly[Encoder, ?, ?], at, a, bt, b] =>
              RightOnly[Encoder, at * bt, (a, b)]((a => x.left.fb(a._1) + "," + x.right.fb(a._2)))
            case x: Sum[RightOnly[Encoder, ?, ?], at, a, bt, b] =>
              RightOnly[Encoder, at + bt, a \/ b](a => a.fold(x.left.fb, x.right.fb))
            case i: IsoSchema[RightOnly[Encoder, ?, ?], _, _, _] =>
              RightOnly[Encoder, AT, A](i.base.fb.compose(i.iso.reverseGet))
            case r: Record[RightOnly[Encoder, ?, ?], _, _] =>
              RightOnly[Encoder, AT, A](
                encloseInBraces.compose(r.fields.fb).compose(r.iso.reverseGet)
              )
            case x: Sequence[RightOnly[Encoder, ?, ?], _, a] =>
              RightOnly[Encoder, AT, List[a]](
                (a: List[a]) => a.map(x.element.fb).mkString("[", ",", "]")
              )
            case x: Field[RightOnly[Encoder, ?, ?], AT, A] =>
              RightOnly[Encoder, AT, A](makeField(fieldLabel(x.id)).compose(x.schema.fb))
            case u: Union[RightOnly[Encoder, ?, ?], AT, A] =>
              RightOnly[Encoder, AT, A](
                encloseInBraces.compose(u.choices.fb).compose(u.iso.reverseGet)
              )
            case x: Branch[RightOnly[Encoder, ?, ?], AT, A] =>
              RightOnly[Encoder, AT, A](makeField(branchLabel(x.id)).compose(x.schema.fb))
            case One()                     => RightOnly[Encoder, AT, A]((_ => "null"))
            case ref @ SelfReference(_, _) => RightOnly[Encoder, AT, A]((a => ref.unroll.fb(a)))
            case ConstSchemaF(base, a)     => RightOnly[Encoder, AT, A](_ => base.fb(a))
          }
      }
    )
}
