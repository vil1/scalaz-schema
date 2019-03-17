package scalaz

package schema

package tests

import monocle.Iso
import testz._

object SchemaModuleExamples {

  def tests[T](harness: Harness[T]): T = {
    import harness._

    val jsonModule = new SchemaModule[JsonSchema](JsonSchema) with JsonModule[JsonSchema] {}

    import jsonModule._, R._

    section("Manipulating Schemas")(
      test("imap on IsoSchema shouldn't add new layer") { () =>
        val adminToListIso  = Iso[Admin, List[String]](_.rights)(Admin.apply)
        def listToSeqIso[A] = Iso[List[A], Seq[A]](_.toSeq)(_.toList)

        val adminSchema = record(
          "rights" -*>: seq(prim(JsonString)),
          Iso[List[String], Admin](Admin.apply)(_.rights)
        )

        adminSchema.imap(adminToListIso).imap(listToSeqIso).unFix match {
          case IsoSchemaF(base, _) => assert(base == adminSchema)
          case _                   => assert(false)
        }
      }
    )
  }
}
