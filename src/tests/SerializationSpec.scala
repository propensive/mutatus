package mutatus.tests

import com.google.cloud.datastore._
import mutatus._
import probably._
import Mutatus._

case class SerializationSpec(implicit runner: Runner) {
  import SerializationSpec.Model._

  {
    val encoder = implicitly[Encoder[Option[String]]]
    test("encodes optionals - empty")(encoder.encode(Option.empty[String]))
      .assert(_ == NullValue.of())
    test("encodes optionals - non-empty")(encoder.encode(Some("content")))
      .assert(_ == StringValue.of("content"))
  }

  {
    val decoder = implicitly[Decoder[Option[String]]]
    test("decodes optionals - empty")(decoder.decodeValue(NullValue.of()))
      .assert {
        case Answer(value) => value.isEmpty
        case _             => false
      }
    test("decodes optionals - non-empty")(
      decoder.decodeValue(StringValue.of("content"))
    ).assert {
      case Answer(Some("content")) => true
      case _                       => false
    }
  }

  for {
    (items, expected) <- List(
      List.empty[Int] -> ListValue.newBuilder().build(),
      List(1, 2, 3) -> ListValue.of(1, 2, 3)
    )
    encodedValue <- List(
      implicitly[Encoder[List[Int]]].encode(items.toList),
      implicitly[Encoder[Stream[Int]]].encode(items.toStream),
      implicitly[Encoder[Vector[Int]]].encode(items.toVector),
      implicitly[Encoder[Set[Int]]].encode(items.toSet)
    )
  } test(
    s"encodes collections - ${if (items.isEmpty) "empty" else "non-empty"}"
  )(
    encodedValue
  ).assert {
    case value: ListValue => value == expected
    case other            => false
  }

  for {
    (listValue, items) <- List(
      ListValue.newBuilder().build() -> Nil,
      ListValue.of(1, 2, 3) -> List(1, 2, 3)
    )
    (decodedValue, expected) <- List(
      implicitly[Decoder[List[Int]]].decodeValue(listValue) -> items.toList,
      implicitly[Decoder[Stream[Int]]].decodeValue(listValue) -> items.toStream,
      implicitly[Decoder[Vector[Int]]].decodeValue(listValue) -> items.toVector,
      implicitly[Decoder[Set[Int]]].decodeValue(listValue) -> items.toSet
    )
  } test(
    s"decodes collections - ${if (items.isEmpty) "empty" else "non-empty"}"
  )(
    decodedValue
  ).assert {
    case Answer(value) => value == expected
    case _             => false
  }

  val entityStateEncoder = implicitly[Encoder[EntityState]]
  for {
    (value, expected) <- List(
      EntityState.WithoutContext -> FullEntity.newBuilder
        .set(
          Encoder.metaField,
          EntityValue
            .newBuilder(
              FullEntity
                .newBuilder()
                .set(Encoder.typenameField, "WithoutContext")
                .build
            )
            .setExcludeFromIndexes(true)
            .build()
        )
        .build,
      EntityState.WithoutContext2 -> FullEntity.newBuilder
        .set(
          Encoder.metaField,
          EntityValue
            .newBuilder(
              FullEntity
                .newBuilder()
                .set(
                  Encoder.typenameField,
                  "WithoutContext2"
                )
                .build
            )
            .setExcludeFromIndexes(true)
            .build()
        )
        .build,
      EntityState.WithContext(123, Math.PI) -> FullEntity
        .newBuilder()
        .set("param1", 123)
        .set("param2", Math.PI)
        .set(
          Encoder.metaField,
          EntityValue
            .newBuilder(
              FullEntity
                .newBuilder()
                .set(
                  Encoder.typenameField,
                  "WithContext"
                )
                .build()
            )
            .setExcludeFromIndexes(true)
            .build()
        )
        .build
    )
  } {
    test("encodes sealed traits")(
      implicitly[Encoder[EntityState]].encode(value)
    ).assert(_ == EntityValue.of(expected))
    test("decodes sealed traits")(
      implicitly[Decoder[EntityState]].decodeValue(EntityValue.of(expected))
    ).assert {
      case Answer(decoded) => decoded == value
      case _               => false
    }
  }

  {
    val simple = Simple(
      intParam = 123,
      stringParam = "456",
      optParam = Some("777"),
      listParam = List("8", "9")
    )

    val expectedSimpleEntity = EntityValue.of {
      Simple.builder
        .set("intParam", 123)
        .set("stringParam", "456")
        .set("optParam", "777")
        .set("listParam", ListValue.of("8", "9"))
        .build()
    }

    test("encodes case classes - simple")(
      implicitly[Encoder[Simple]].encode(simple)
    ).assert(_ == expectedSimpleEntity)

    val complex = Complex(
      simple = simple,
      optSimple = Some(simple),
      list = List(simple, simple)
    )

    val expectedComplexEntity = EntityValue.of {
      Simple.builder
        .set("simple", expectedSimpleEntity)
        .set("optSimple", expectedSimpleEntity)
        .set("list", ListValue.of(expectedSimpleEntity, expectedSimpleEntity))
        .set(
          "state",
          FullEntity.newBuilder
            .set(
              Encoder.metaField,
              EntityValue
                .newBuilder(
                  FullEntity
                    .newBuilder()
                    .set(
                      Encoder.typenameField,
                      "WithoutContext"
                    )
                    .build
                )
                .setExcludeFromIndexes(true)
                .build()
            )
            .build
        )
        .build()
    }

    test("encodes case classes - complex")(
      implicitly[Encoder[Complex]].encode(complex)
    ).assert(_ == expectedComplexEntity)
  }

  {
    val simple = Simple(123, "456", None, Nil)

    def entityBuilder =
      Simple.builder
        .set("intParam", simple.intParam)
        .set("stringParam", simple.stringParam)

    for {
      (encoded, expected) <- List(
        EntityValue.of(entityBuilder.build) -> simple,
        EntityValue.of(entityBuilder.setNull("optParam").build()) -> simple
          .copy(
            optParam = None
          ),
        EntityValue
          .of(entityBuilder.set("optParam", "content").build()) -> simple
          .copy(optParam = Some("content")),
        EntityValue.of(
          entityBuilder
            .set("listParam", ListValue.of("param1", "param2"))
            .build()
        ) -> simple.copy(listParam = List("param1", "param2"))
      )
    } test("decodes case classes - simple")(
      implicitly[Decoder[Simple]].decodeValue(encoded)
    ).assert(x => {
      val passed = x == Answer(expected)
      if (!passed) {
        println(s"""Expected: ${expected}
                   |Actual:   ${x}
                   |Input:    ${encoded}
        """.stripMargin)
      }
      passed
    })

    val expectedSimple = Simple(123, "456", Some("777"), List("8", "9"))
    val simpleEncoded = Simple.builder
      .set("intParam", expectedSimple.intParam)
      .set("stringParam", expectedSimple.stringParam)
      .set("optParam", expectedSimple.optParam.get)
      .set("listParam", ListValue.of("8", "9"))
      .build()

    val complexEncoded = EntityValue.of {
      Simple.builder
        .set("simple", simpleEncoded)
        .set("optSimple", simpleEncoded)
        .set("list", ListValue.of(simpleEncoded, simpleEncoded))
        .build()
    }
    val expectedComplex = Complex(
      expectedSimple,
      Some(expectedSimple),
      List(expectedSimple, expectedSimple)
    )

    test("decodes case classes - complex")(
      implicitly[Decoder[Complex]].decodeValue(complexEncoded)
    ).assert(_ == Answer(expectedComplex))
  }

  {
    test("allows to encode field as not indexed") {
      case class EntityWithNotIndexedFields(foo: Int, bar: String, @notIndexed fooBar: Long)
      implicitly[Encoder[EntityWithNotIndexedFields]].encode{
        EntityWithNotIndexedFields(2, "two", 2L)
      }
    }.assert{
      case value: EntityValue =>  value.get().getValue[LongValue]("fooBar").excludeFromIndexes() && 
        !value.get().getValue[StringValue]("bar").excludeFromIndexes()
    }
  }
}

object SerializationSpec {
  object Model {

    sealed trait EntityState
    object EntityState {
      case object WithoutContext extends EntityState
      case object WithoutContext2 extends EntityState
      case class WithContext(param1: Int, param2: Double) extends EntityState
    }

    case class Simple(
        intParam: Int,
        stringParam: String,
        optParam: Option[String] = None,
        listParam: List[String] = Nil
    )

    object Simple {
      def builder: FullEntity.Builder[_] = FullEntity.newBuilder()
      def entityBuilder: Entity.Builder =
        Entity.newBuilder(Key.newBuilder("unit-tests", "simple", "1").build())
    }

    case class Complex(
        simple: Simple,
        optSimple: Option[Simple] = None,
        list: List[Simple] = Nil,
        state: EntityState = EntityState.WithoutContext
    )
  }
}
