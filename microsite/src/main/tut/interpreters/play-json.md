---
layout: docs
title: "Play JSON"
section: interpreters
---

# {{page.title}}

The `scalaz-schema-play-json` provides interpreters targetting `play.api.libs.json.Reads` and `play.api.libs.json.Writes`.

## Example usage

A module providing these interpreters can be built by mixing the `scalaz.schema.play.json.PlayJsonModule` trait.

In the following example, we also mix the `scalaz.schema.tests.TestModule` trait that brings in  

```tut:silent
import scalaz._, schema.{ play => _, _}, tests._
import scalaz.schema.play.json._
import play.api.libs.json._

object ExampleModule extends PlayJsonModule[JsonSchema.type] with TestModule {
  
  // Notice that we don't need to define the R variable
  // because it is already defined in TestModule. 
  // Otherwise we would have needed to write:
  //
  // val R = JsonSchema
  
  
  implicit val jsonPrimWrites = new (JsonSchema.Prim ~> Writes) {
    def apply[A](p: JsonSchema.Prim[A]): Writes[A] = p match {
      case JsonSchema.JsonNull   => Writes(_ => JsNull)
      case JsonSchema.JsonBool   => Writes(b => JsBoolean(b))
      case JsonSchema.JsonNumber => Writes(n => JsNumber(n))
      case JsonSchema.JsonString => Writes(s => JsString(s))
    }
  }

  implicit val jsonPrimReads = new (JsonSchema.Prim ~> Reads) {
    def apply[A](p: JsonSchema.Prim[A]): Reads[A] = p match {
      case JsonSchema.JsonNull =>
        Reads {
          case JsNull => JsSuccess(())
          case _      => JsError("expected 'null'")
        }
      case JsonSchema.JsonBool   => JsPath.read[Boolean]
      case JsonSchema.JsonString => JsPath.read[String]
      case JsonSchema.JsonNumber => JsPath.read[BigDecimal]
    }

  }

}
```

As usual, we need to provide natural transformations from our set of primitive types to our target functors (`Reads` and `Writes`).

Now we can use this module to interpret a schema (for example, the `person` schema defined in the `tests` module).

```tut:silent
import ExampleModule._

val personReads = person.to[Reads]

val personWrites = person.to[Writes]

val homer = Person(name = "Homer", role = Some(Admin(List("prod", "dev"))))
```

```tut
val json = personWrites.writes(homer)

val fromJson = personReads.reads(json)

fromJson == JsSuccess(homer)
```

## Encoding for unions

Union branches are encoded as JSON objects with a single field whose name corresponds to the branch's id.

In the example above, the `Role` type is a sealed trait (and is therefore represented as an union), we can see that its value is encoded as `{"admin":{"rights":["prod","dev"]}}`, where `admin` is the id defined for the branch corresponding to the `Admin` class (which is one of `Role`'s subclasses).

## Encoding for tuples

JSON doesn't define any standard way to represent tuples. Although it is quite unusual to (de)serialize mere tuples to/from JSON, we need to account for that case (interpreters must be able to handle any possible schema). 

We decided to encode tuples as "nested pairs" (mostly because it makes implementing interpreters easier). For example, lets consider this schema representing a triple:

```tut:silent
val nestedTriple = monocle.Iso[(Boolean, (BigDecimal, String)), (Boolean, BigDecimal, String)](n => (n._1, n._2._1, n._2._2))(t => (t._1, (t._2, t._3)))

val triple = iso(prim(JsonSchema.JsonBool) :*: prim(JsonSchema.JsonNumber) :*: prim(JsonSchema.JsonString), nestedTriple)

val tripleWrites = triple.to[Writes]
```

```tut
tripleWrites.writes((true, BigDecimal(42), "foo"))
```

As you can see, the triple is represented as nested JSON objects with a `_1` and `_2` field each. 
