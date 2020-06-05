// package mutatus.tests

// import mutatus._
// import probably._
// import com.google.cloud.datastore._
// import util.Try
// import Mutatus._

// case class EndToEndSpec()(implicit runner: Runner) {
//   implicit val datastore: Datastore =
//     DatastoreOptions.getDefaultInstance.getService
//   import EndToEndSpec.Model._

//   val simpleEntities: Vector[TestSimpleEntity] = for {
//     idx <- 1.until(10).toVector
//   } yield TestSimpleEntity(
//     id = idx,
//     longParam = idx * Int.MaxValue.toLong,
//     decimalParam = idx * 0.25,
//     stringParam = s"simple-$idx",
//     optParam = Some(s"param-$idx").filter(_ => idx % 2 == 0),
//     list = 1.until(idx).toList,
//     state = SomeState(idx)
//   )
//   val batchedSimpleEntities: Vector[TestSimpleEntity] =
//     simpleEntities.map(e => e.copy(id = e.id + 1e6.toInt))
//   def generateComplex[Id](idFn: Int => Id)(idx: Int): TestComplex[Id] = {
//     val listOptInt =
//       0.until(idx).map(Some.apply).filter(_.exists(_ % 2 == 0)).toList
//     val listOptInner = 0
//       .until(idx)
//       .toList
//       .map(n =>
//         Some(
//           Inner(
//             int = n,
//             string = s"inner-$n",
//             optDecimal = Some(n * Math.PI).filter(_ => n % 3 == 0),
//             simpleList = simpleEntities.take(n).toList,
//             state = SomeState(n)
//           )
//         ).filter(_.int % 4 != 0)
//       )

//     {
//       idFn(idx) match {
//         case id: String =>
//           TestComplexStringId(
//             id,
//             listOptInt,
//             listOptInner.lastOption.flatten,
//             listOptInner
//           )
//         case id: Long =>
//           TestComplexLongId(
//             id,
//             listOptInt,
//             listOptInner.lastOption.flatten,
//             listOptInner
//           )
//         case id: Guid =>
//           TestComplexGuid(
//             id,
//             listOptInt,
//             listOptInner.lastOption.flatten,
//             listOptInner
//           )
//       }
//     }.asInstanceOf[TestComplex[Id]]
//   }

//   val longIdComplexEntities = 0
//     .until(10)
//     .map(generateComplex[Long](_ + 1e6.toLong))
//     .toVector
//     .asInstanceOf[Vector[TestComplexLongId]]
//   val stringIdComplexEntities =
//     0.until(10)
//       .map(generateComplex[String](id => s"very-secure-id-$id"))
//       .toVector
//       .asInstanceOf[Vector[TestComplexStringId]]
//   val guidComplexEntities =
//     0.until(10)
//       .map(generateComplex[Guid](_ => Guid()))
//       .toVector
//       .asInstanceOf[Vector[TestComplexGuid]]

//   simpleEntities.foreach { e =>
//     test("save entities - simple")(e.save().map(_.ref.getId()))
//       .assert(_ == Answer(e.id))
//   }
//   longIdComplexEntities.foreach { e =>
//     test("save entities - complex - long id")(e.save().map(_.ref.getId()))
//       .assert(
//         _ == Answer(e.id)
//       )
//   }
//   stringIdComplexEntities.foreach { e =>
//     test("save entities - complex - string id")(e.save().map(_.ref.getName()))
//       .assert(_ == Answer(e.id))
//   }

//   guidComplexEntities.foreach { e =>
//     test("save entities - complex - guid")(e.save().map(_.ref.getName()))
//       .assert(_ == Answer(e.id.guid))
//   }

//   test("save entites in batch mode")(batchedSimpleEntities.saveAll()).assert {
//     case Answer(result) =>
//       result.nonEmpty && result.forall {
//         case (ent, ref) => ref.ref.getId() == ent.id
//       }
//     case _ => false
//   }

//   test("fetch entities - simple")(
//     Dao[TestSimpleEntity].all.run()
//   ).assert(
//     _.toVector == (simpleEntities ++ batchedSimpleEntities)
//       .sortBy(_.id)
//       .map(Answer.apply(_))
//   )

//   test("fetch entities - complex - long id")(
//     Dao[TestComplexLongId].all.run()
//   ).assert(
//     _.toVector == longIdComplexEntities.toVector
//       .sortBy(_.id)
//       .map(Answer.apply)
//   )

//   simpleEntities.take(3).foreach { e =>
//     test("fetch entity by id - simple")(Dao[TestSimpleEntity].unapply(e.id))
//       .assert(_.contains(Answer(e)))
//   }
//   longIdComplexEntities
//     .take(3)
//     .foreach(e =>
//       test("fetch entity by id - complex - longId")(
//         Dao[TestComplexLongId].unapply(e.id)
//       ).assert(_.contains(Answer(e)))
//     )
//   stringIdComplexEntities
//     .take(3)
//     .foreach(e =>
//       test("fetch entity by id - complex - stringId")(
//         Dao[TestComplexStringId].unapply(e.id)
//       ).assert(_.contains(Answer(e)))
//     )
//   guidComplexEntities
//     .take(3)
//     .foreach(e =>
//       test("fetch entity by id - complex - guild")(
//         Dao[TestComplexGuid].unapply(e.id)
//       ).assert(_.contains(Answer(e)))
//     )

//   test("allows to fetch using queries") {
//     Dao[TestComplexLongId].all
//       .filter(_.innerOpt.exists(_.int >= 2))
//       .filter(_.innerOpt.exists(_.int <= 8))
//       .sortBy(_.innerOpt.map(_.int))
//       .reverse
//       .drop(1)
//       .take(2)
//       .run()
//       .toList
//   }.assert(
//     _ == longIdComplexEntities
//       .filter(_.innerOpt.exists(_.int >= 2))
//       .filter(_.innerOpt.exists(_.int <= 8))
//       .sortBy(_.innerOpt.map(_.int).getOrElse(-1))(
//         implicitly[Ordering[Int]].reverse
//       )
//       .drop(1)
//       .take(2)
//       .toList
//       .map(Answer(_))
//   )

//   simpleEntities.take(5).foreach { entity =>
//     val updated = entity.copy(
//       decimalParam = entity.decimalParam * 10.0,
//       longParam = entity.longParam / 2,
//       optParam = entity.optParam.map(_ * 2)
//     )
//     test("updates entities") {
//       updated.save()
//       Dao[TestSimpleEntity].unapply(entity.id)
//     }.assert(_.contains(Answer(updated)))
//   }

//   {
//     def fetchEntities = Dao[TestSimpleEntity].all.run()
//     val simpleEntitiesBefore = fetchEntities
//     val error = Error(SerializationException("Mocked up problem"))

//     test("rollbacks transactions in case of failure") {
//       Dao.transaction { implicit tx =>
//         for {
//           updated <- fetchEntities
//             .collect {
//               case Answer(value) => value.copy(longParam = value.longParam + 1L)
//             }
//             .saveAll()
//           _ <- error
//         } yield updated
//       }
//     }.assert(result => {
//       val stateAfter = fetchEntities
//       (simpleEntitiesBefore, result, stateAfter) match {
//         case (before, `error`, after) =>
//           before.toList == after.toList
//         case _ => false
//       }
//     })
//   }

//   {
//     def fetchEntities = Dao[TestSimpleEntity].all.run()
//     test("commits transactions in case of success") {
//       Dao.transaction { implicit tx =>
//         for {
//           updated <- fetchEntities
//             .collect {
//               case Answer(value) => value.copy(longParam = value.longParam + 2L)
//             }
//             .saveAll()
//         } yield updated
//       }
//     }.assert(result => {
//       val stateAfter = fetchEntities
//         .collect {
//           case Answer(value) => value
//         }
//         .toList
//         .sortBy(_.id)

//       (stateAfter, result.map(_.keys.toList.sortBy(_.id))) match {
//         case (expected, Answer(actual)) =>
//           val passed = expected == actual
//           if (!passed) {
//             println(s"""
//               Expected: ${expected}
//               Actual:   ${actual}
//             """)
//           }
//           passed
//         case (expected, actual) =>
//           println(s"""
//               Expected: ${expected}
//               Actual:   ${actual}
//             """)
//           false
//       }
//     })
//   }

//   def fetchSimpleEntities() = Dao[TestSimpleEntity].all.run()

//   {
//     test("submits batches") {
//       val stateBefore = fetchSimpleEntities
//       Dao.batch { implicit batch =>
//         for {
//           x <- Result {
//             stateBefore
//               .take(5)
//               .collect {
//                 case Answer(value) =>
//                   val updated =
//                     value.copy(decimalParam = value.decimalParam + 1.0)
//                   value -> value.save()
//               }
//               .toMap
//           }
//           y <- stateBefore
//             .drop(5)
//             .collect {
//               case Answer(value) =>
//                 value.copy(decimalParam = value.decimalParam - 1.0)
//             }
//             .saveAll()
//         } yield x ++ y
//       }
//     }.assert(result => {
//       val actualState = fetchSimpleEntities().collect {
//         case Answer(value) => value
//       }

//       (result, actualState) match {
//         case (Answer(expected), actual) =>
//           expected.keySet.toList.sortBy(_.id) == actual.toList
//         case _ => false
//       }
//     })

//     val stateBefore = fetchSimpleEntities()
//     val error = Error(SerializationException("Mock up"))
//     test("does not submit errorues batches") {
//       Dao.batch { implicit batch =>
//         for {
//           updated <- Result {
//             stateBefore.collect {
//               case Answer(value) =>
//                 value.copy(decimalParam = value.decimalParam + Math.PI)
//             }
//           }
//           _ <- updated.saveAll()
//           _ <- error
//         } yield updated
//       }
//     }.assert(result =>
//       (result, stateBefore, fetchSimpleEntities()) match {
//         case (`error`, stateBefore, actualState) =>
//           stateBefore.nonEmpty && stateBefore.toList == actualState.toList
//         case _ => false
//       }
//     )
//   }

//   simpleEntities.foreach { e =>
//     test("removes entities") {
//       e.delete()
//         .map { _ => Dao[TestSimpleEntity].unapply(e.id) }
//     }.assert {
//       case Answer(value) => value.isEmpty
//       case _             => false
//     }
//   }

//   longIdComplexEntities.foreach { e =>
//     test("removes entities") {
//       e.delete()
//         .map { _ => Dao[TestComplexLongId].unapply(e.id) }
//     }.assert {
//       case Answer(value) => value.isEmpty
//       case _             => false
//     }
//   }

//   stringIdComplexEntities.foreach { e =>
//     test("removes entities") {
//       e.delete().map { _ => Dao[TestComplexStringId].unapply(e.id) }
//     }.assert {
//       case Answer(value) => value.isEmpty
//       case _             => false
//     }
//   }

//   guidComplexEntities.foreach { e =>
//     test("removes entities") {
//       e.delete().map { _ => Dao[TestComplexGuid].unapply(e.id) }
//     }.assert {
//       case Answer(value) => value.isEmpty
//       case _             => false
//     }
//   }

//   test("removes entities in batch mode") {
//     batchedSimpleEntities.deleteAll()
//     Dao[TestSimpleEntity].all.run()
//   }.assert(_.isEmpty)

//   test("removed everything") {
//     List(
//       Dao[TestSimpleEntity].all.run(),
//       Dao[TestComplexLongId].all.run(),
//       Dao[TestComplexStringId].all.run(),
//       Dao[TestComplexGuid].all.run()
//     )
//   }.assert(_.forall(_.isEmpty))
// }

// object EndToEndSpec {
//   object Model {
//     sealed trait SomeState
//     object SomeState {
//       def apply(idx: Int): SomeState =
//         if (idx <= 2) MaybeOk
//         else if (idx > 6) Ok
//         else MarkedAs(idx)

//       case object Ok extends SomeState
//       case object MaybeOk extends SomeState
//       case class MarkedAs(mark: Int) extends SomeState
//     }
//     case class TestSimpleEntity(
//         @id id: Int,
//         longParam: Long,
//         decimalParam: Double,
//         stringParam: String,
//         optParam: Option[String],
//         list: List[Int],
//         state: SomeState
//     )

//     case class Inner(
//         int: Int,
//         string: String,
//         optDecimal: Option[Double],
//         simpleList: List[TestSimpleEntity],
//         state: SomeState
//     )

//     sealed trait TestComplex[Id] {
//       def id: Id
//       def listOptInt: List[Option[Int]]
//       def listOptInner: List[Option[Inner]]
//       def innerOpt: Option[Inner]
//     }
//     case class TestComplexStringId(
//         @id id: String,
//         listOptInt: List[Option[Int]],
//         innerOpt: Option[Inner],
//         listOptInner: List[Option[Inner]]
//     ) extends TestComplex[String]

//     case class TestComplexLongId(
//         @id id: Long,
//         listOptInt: List[Option[Int]],
//         innerOpt: Option[Inner],
//         listOptInner: List[Option[Inner]]
//     ) extends TestComplex[Long]

//     case class TestComplexGuid(
//         @id id: Guid,
//         listOptInt: List[Option[Int]],
//         innerOpt: Option[Inner],
//         listOptInner: List[Option[Inner]]
//     ) extends TestComplex[Guid]
//   }
// }
