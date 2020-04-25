package mutatus.tests

import mutatus._
import probably._
import com.google.cloud.datastore._
import util.Try

case class EndToEndSpec()(implicit runner: Runner) {
  implicit val datastore: Datastore =
    DatastoreOptions.getDefaultInstance.getService
  import EndToEndSpec.Model._

  val simpleEntities: Vector[TestSimpleEntity] = for {
    idx <- 1.until(10).toVector
  } yield TestSimpleEntity(
    id = idx,
    longParam = idx * Int.MaxValue.toLong,
    decimalParam = idx * 0.25,
    stringParam = s"simple-$idx",
    optParam = Some(s"param-$idx").filter(_ => idx % 2 == 0),
    list = 1.until(idx).toList,
    state = SomeState(idx)
  )
  val batchedSimpleEntities: Vector[TestSimpleEntity] =
    simpleEntities.map(e => e.copy(id = e.id + 1e6.toInt))
  def generateComplex[Id](idFn: Int => Id)(idx: Int): TestComplex[Id] = {
    val listOptInt =
      0.until(idx).map(Some.apply).filter(_.exists(_ % 2 == 0)).toList
    val listOptInner = 0
      .until(idx)
      .toList
      .map(n =>
        Some(
          Inner(
            int = n,
            string = s"inner-$n",
            optDecimal = Some(n * Math.PI).filter(_ => n % 3 == 0),
            simpleList = simpleEntities.take(n).toList,
            state = SomeState(n)
          )
        ).filter(_.int % 4 != 0)
      )

    {
      idFn(idx) match {
        case id: String =>
          TestComplexStringId(
            id,
            listOptInt,
            listOptInner.lastOption.flatten,
            listOptInner
          )
        case id: Long =>
          TestComplexLongId(
            id,
            listOptInt,
            listOptInner.lastOption.flatten,
            listOptInner
          )
        case id: Guid =>
          TestComplexGuid(
            id,
            listOptInt,
            listOptInner.lastOption.flatten,
            listOptInner
          )
      }
    }.asInstanceOf[TestComplex[Id]]
  }

  val longIdComplexEntities = 0
    .until(10)
    .map(generateComplex[Long](_ + 1e6.toLong))
    .toVector
    .asInstanceOf[Vector[TestComplexLongId]]
  val stringIdComplexEntities =
    0.until(10)
      .map(generateComplex[String](id => s"very-secure-id-$id"))
      .toVector
      .asInstanceOf[Vector[TestComplexStringId]]
  val guidComplexEntities =
    0.until(10)
      .map(generateComplex[Guid](_ => Guid()))
      .toVector
      .asInstanceOf[Vector[TestComplexGuid]]

  simpleEntities.foreach { e =>
    test("save entities - simple")(e.save().map(_.ref.getId()))
      .assert(_ == Answer(e.id))
  }
  longIdComplexEntities.foreach { e =>
    test("save entities - complex - long id")(e.save().map(_.ref.getId()))
      .assert(
        _ == Answer(e.id)
      )
  }
  stringIdComplexEntities.foreach { e =>
    test("save entities - complex - string id")(e.save().map(_.ref.getName()))
      .assert(_ == Answer(e.id))
  }

  guidComplexEntities.foreach { e =>
    test("save entities - complex - guid")(e.save().map(_.ref.getName()))
      .assert(_ == Answer(e.id.guid))
  }

  test("save entites in batch mode")(batchedSimpleEntities.saveAll()).assert {
    case Answer(result) =>
      result.nonEmpty && result.forall {
        case (ent, ref) => ref.ref.getId() == ent.id
      }
    case _ => false
  }

//   test("fetch entities - simple")(
//     Dao[TestSimpleEntity].all.run()
//   ).assert {
//     case Answer(value) =>
//       value.toVector == (simpleEntities ++ batchedSimpleEntities)
//         .sortBy(_.id)
//         .map(mutatus.Answer(_))
//     case _ => false
//   }

//   test("fetch entities - complex - long id")(
//     Dao[TestComplexLongId].all.run()
//   ).assert {
//     case Answer(value) =>
//       value.toVector == longIdComplexEntities.toVector
//         .sortBy(_.id)
//         .map(Answer.apply)
//     case _ => false
//   }

  simpleEntities.take(3).foreach { e =>
    test("fetch entity by id - simple")(Dao[TestSimpleEntity].unapply(e.id))
      .assert(_.contains(Answer(e)))
  }
  longIdComplexEntities
    .take(3)
    .foreach(e =>
      test("fetch entity by id - complex - longId")(
        Dao[TestComplexLongId].unapply(e.id)
      ).assert(_.contains(Answer(e)))
    )
  stringIdComplexEntities
    .take(3)
    .foreach(e =>
      test("fetch entity by id - complex - stringId")(
        Dao[TestComplexStringId].unapply(e.id)
      ).assert(_.contains(Answer(e)))
    )
  guidComplexEntities
    .take(3)
    .foreach(e =>
      test("fetch entity by id - complex - guild")(
        Dao[TestComplexGuid].unapply(e.id)
      ).assert(_.contains(Answer(e)))
    )
//   test("allows to fetch using queries") {
//     Dao[TestComplexLongId].all
//       .filter(_.innerOpt.exists(_.int >= 2))
//       .filter(_.innerOpt.exists(_.int <= 8))
//       .sortBy(_.innerOpt.map(_.int))
//       .reverse
//       .drop(1)
//       .take(2)
//       .run()
//       .map(_.toList)
//   }.assert(
//     _ == Answer(
//       longIdComplexEntities
//         .filter(_.innerOpt.exists(_.int >= 2))
//         .filter(_.innerOpt.exists(_.int <= 8))
//         .sortBy(_.innerOpt.map(_.int).getOrElse(-1))(
//           implicitly[Ordering[Int]].reverse
//         )
//         .drop(1)
//         .take(2)
//         .toList
//         .map(Answer(_))
//     )
//   )

  simpleEntities.take(5).foreach { entity =>
    val updated = entity.copy(
      decimalParam = entity.decimalParam * 10.0,
      longParam = entity.longParam / 2,
      optParam = entity.optParam.map(_ * 2)
    )
    test("updates entities") {
      updated.save()
      Dao[TestSimpleEntity].unapply(entity.id)
    }.assert(_.contains(Answer(updated)))
  }

  simpleEntities.foreach { e =>
    test("removes entities") {
      e.delete()
        .map { _ => Dao[TestSimpleEntity].unapply(e.id) }
    }.assert {
      case Answer(value) => value.isEmpty
      case _             => false
    }
  }

  longIdComplexEntities.foreach { e =>
    test("removes entities") {
      e.delete()
        .map { _ => Dao[TestComplexLongId].unapply(e.id) }
    }.assert {
      case Answer(value) => value.isEmpty
      case _             => false
    }
  }

  stringIdComplexEntities.foreach { e =>
    test("removes entities") {
      e.delete().map { _ => Dao[TestComplexStringId].unapply(e.id) }
    }.assert {
      case Answer(value) => value.isEmpty
      case _             => false
    }
  }

  guidComplexEntities.foreach { e =>
    test("removes entities") {
      e.delete().map { _ => Dao[TestComplexGuid].unapply(e.id) }
    }.assert {
      case Answer(value) => value.isEmpty
      case _             => false
    }
  }

//   test("removes entities in batch mode") {
//     batchedSimpleEntities.deleteAll()
//     Dao[TestSimpleEntity].all.run()
//   }.assert {
//     case Answer(result) => result.isEmpty
//     case _              => false
//   }

//   test("removed everything") {
//     List(
//       Dao[TestSimpleEntity].all.run(),
//       Dao[TestComplexLongId].all.run(),
//       Dao[TestComplexStringId].all.run(),
//       Dao[TestComplexGuid].all.run()
//     )
//   }.assert(_.forall {
//     case Answer(result) => result.isEmpty
//     case other =>
//       println(other)
//       false
//   })

}

object EndToEndSpec {
  object Model {
    sealed trait SomeState
    object SomeState {
      def apply(idx: Int): SomeState =
        if (idx <= 2) MaybeOk
        else if (idx > 6) Ok
        else MarkedAs(idx)

      case object Ok extends SomeState
      case object MaybeOk extends SomeState
      case class MarkedAs(mark: Int) extends SomeState
    }
    case class TestSimpleEntity(
        @id id: Int,
        longParam: Long,
        decimalParam: Double,
        stringParam: String,
        optParam: Option[String],
        list: List[Int],
        state: SomeState
    )

    case class Inner(
        int: Int,
        string: String,
        optDecimal: Option[Double],
        simpleList: List[TestSimpleEntity],
        state: SomeState
    )

    sealed trait TestComplex[Id] {
      def id: Id
      def listOptInt: List[Option[Int]]
      def listOptInner: List[Option[Inner]]
      def innerOpt: Option[Inner]
    }
    case class TestComplexStringId(
        @id id: String,
        listOptInt: List[Option[Int]],
        innerOpt: Option[Inner],
        listOptInner: List[Option[Inner]]
    ) extends TestComplex[String]

    case class TestComplexLongId(
        @id id: Long,
        listOptInt: List[Option[Int]],
        innerOpt: Option[Inner],
        listOptInner: List[Option[Inner]]
    ) extends TestComplex[Long]

    case class TestComplexGuid(
        @id id: Guid,
        listOptInt: List[Option[Int]],
        innerOpt: Option[Inner],
        listOptInner: List[Option[Inner]]
    ) extends TestComplex[Guid]
  }
}
