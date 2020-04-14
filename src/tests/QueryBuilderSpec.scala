package mutatus.tests

import com.google.cloud.NoCredentials
import com.google.cloud.datastore.StructuredQuery._
import com.google.cloud.datastore._
import mutatus.QueryBuilder.OrderDirection
import mutatus.QueryBuilder.OrderDirection._
import mutatus._
import probably._

case class QueryBuilderSpec()(implicit runner: Runner) {
  import QueryBuilderSpec.Model._
  implicit val defaultDataStore = Service {
    DatastoreOptions
      .newBuilder()
      .setProjectId("unit-tests")
      .setCredentials(NoCredentials.getInstance())
      .build()
      .getService
  }
  lazy val builder = Dao[QueringTestEntity].query

  List(
    builder.where(_.intParam == 0) -> PropertyFilter.eq("intParam", 0),
    builder.where(_.innerClass.intParam == 1) -> PropertyFilter.eq(
      "innerClass.intParam",
      1
    ),
    builder.where(_.innerClass.deeper.some2 == 2) -> PropertyFilter.eq(
      "innerClass.deeper.some2",
      2
    ),
    builder.where(_.innerClass.deeper.inner.some3 == 3) -> PropertyFilter.eq(
      "innerClass.deeper.inner.some3",
      3
    ),
    builder.where(_.innerClassOpt.exists(_.intParam == 1)) -> PropertyFilter.eq(
      "innerClassOpt.intParam",
      1
    ),
    builder.where(_.innerClassOpt.exists(_.deeper.some2 == 2)) -> PropertyFilter
      .eq("innerClassOpt.deeper.some2", 2),
    builder.where(_.innerClassOpt.exists(_.deeper.inner.some3 == 3)) -> PropertyFilter
      .eq("innerClassOpt.deeper.inner.some3", 3),
    builder.where(_.innerClassOpt.exists(_.intParam == 1)) -> PropertyFilter.eq(
      "innerClassOpt.intParam",
      1
    ),
    builder.where(_.innerClassOpt.exists(_.deeperOpt.exists(_.some2 == 2))) -> PropertyFilter
      .eq("innerClassOpt.deeperOpt.some2", 2),
    builder.where(
      _.innerClassOpt.exists(_.deeperOpt.exists(_.inner.some3 == 3))
    ) -> PropertyFilter.eq("innerClassOpt.deeperOpt.inner.some3", 3),
    builder.where(
      _.innerClassOpt.exists(
        _.deeperOpt.exists(_.optInner.exists(_.some3 == 3))
      )
    ) -> PropertyFilter.eq("innerClassOpt.deeperOpt.optInner.some3", 3),
    builder.where(_.optionalParam.isEmpty) -> PropertyFilter.isNull(
      "optionalParam"
    ),
    builder.where(_.optionalParam.isDefined) -> PropertyFilter.gt(
      "optionalParam",
      NullValue.of()
    ),
    builder.where(
      _.innerClassOpt.flatMap(_.deeperOpt).map(_.inner.some3).isDefined
    ) -> PropertyFilter
      .gt("innerClassOpt.deeperOpt.inner.some3", NullValue.of()),
    builder.where(_.optionalParam.contains("param")) -> PropertyFilter.eq(
      "optionalParam",
      "param"
    ),
    builder.where(_.innerClass.deeper.optInner.map(_.some3).contains(1)) -> PropertyFilter
      .eq("innerClass.deeper.optInner.some3", 1),
    builder.where(
      _.innerClass.deeperOpt.flatMap(_.optInner).exists(_.some3 > 1)
    ) -> PropertyFilter
      .gt("innerClass.deeperOpt.optInner.some3", 1),
    builder.where(_.innerClassOpt.map(_.decimalParam).contains(2.0)) -> PropertyFilter
      .eq("innerClassOpt.decimalParam", 2.0),
    builder.where(
      _.listParam.init.tail.headOption.head.lastOption.last.toString == "param"
    ) -> PropertyFilter.eq("listParam", "param"),
    builder.where(x =>
      x.listParam.contains("paramName") &&
        x.innerClass.deeper.inner.some3 >= 3 &&
        x.innerClassOpt.map(_.deeper.optInner).exists(_.forall(_.some3 <= 3))
    ) -> CompositeFilter.and(
      PropertyFilter.eq("listParam", "paramName"),
      PropertyFilter.ge("innerClass.deeper.inner.some3", 3),
      PropertyFilter.le("innerClassOpt.deeper.optInner.some3", 3)
    )
  ).zipWithIndex.foreach {
    case ((queryBuilder: QueryBuilder[QueringTestEntity], filter), idx) => {
      test(s"builds query conditions - case $idx")(queryBuilder.filterCriteria)
        .assert { fc =>
          val passed = fc.contains(filter)
          if (!passed) {
            println {
              s"""
              Failed at case $idx
              Expected: ${Some(filter)}
              Actual:   $fc """.stripMargin
            }
          }
          passed
        }
    }
  }

  test("builds multiple cirtiera within lambda") {
    val x = 5
    builder.where(e =>
      e.intParam == x + 1 && e.innerClassOpt.exists { inner =>
        inner.intParam > 0 &&
        inner.optionalParam.exists(_ > 1) &&
        inner.deeper.optInner.exists(x => x.some3 <= 42 && x.none.isDefined)
      }
    )
  }.assert(result => {
    val expected = CompositeFilter.and(
      PropertyFilter.eq("intParam", 6),
      PropertyFilter.gt("innerClassOpt.intParam", 0),
      PropertyFilter.gt("innerClassOpt.optionalParam", 1),
      PropertyFilter.le("innerClassOpt.deeper.optInner.some3", 42),
      PropertyFilter.gt("innerClassOpt.deeper.optInner.none", NullValue.of())
    )
    val passed = result.filterCriteria.contains(expected)
    if (!passed) {
      println(s"""
              Expected: ${Some(expected)}
              Actual:   ${result.filterCriteria}
              """.stripMargin)
    }
    passed
  })

  List(
    builder.orderBy(_.asc(_.intParam)) -> OrderBy.asc("intParam"),
    builder.orderBy(_.desc(_.stringParam)) -> OrderBy.desc("stringParam"),
    builder.orderBy(_.asc(_.innerClass.intParam)) -> OrderBy.asc(
      "innerClass.intParam"
    ),
    builder.orderBy(_.desc(_.innerClassOpt.map(_.intParam))) -> OrderBy.desc(
      "innerClassOpt.intParam"
    ),
    builder.orderBy(_.asc(_.innerClass.deeper.some2)) -> OrderBy.asc(
      "innerClass.deeper.some2"
    ),
    builder
      .orderBy(_.desc(_.innerClassOpt.map(_.deeperOpt).map(_.map(_.some2)))) -> OrderBy
      .desc("innerClassOpt.deeperOpt.some2"),
    builder
      .orderBy(_.desc(_.innerClassOpt.flatMap(_.deeperOpt).map(_.some2))) -> OrderBy
      .desc("innerClassOpt.deeperOpt.some2"),
    builder.orderBy(_.asc(_.innerClass.deeper.inner.some3)) -> OrderBy.asc(
      "innerClass.deeper.inner.some3"
    ),
    builder.orderBy(
      _.desc(
        _.innerClassOpt.flatMap(_.deeperOpt).flatMap(_.optInner).map(_.some3)
      )
    ) -> OrderBy.desc("innerClassOpt.deeperOpt.optInner.some3")
  ).zipWithIndex.foreach {
    case ((query: QueryBuilder[QueringTestEntity], expected), idx) =>
      test("build orderBy criteria")(query.orderCriteria)
        .assert(orderCriteria => {
          orderCriteria.size == 1 &&
            orderCriteria.head == expected
        }, _ => s"failed at case $idx, expected result: $expected")
  }

  test("builds multiple orderBy criteria") {
    builder.orderBy(
      _.asc(_.intParam),
      _.desc(_.innerClass.deeperOpt.flatMap(_.optInner).map(_.some3)),
      _.asc(_.innerClassOpt.exists(_.deeper.some2 == 1))
    )
  }.assert { query =>
    query.orderCriteria.toList == List(
      OrderBy.asc("intParam"),
      OrderBy.desc("innerClass.deeperOpt.optInner.some3"),
      OrderBy.asc("innerClassOpt.deeper.some2")
    )
  }

  def pathToOrder(direction: OrderDirection)(path: String) = direction match {
    case Ascending  => OrderBy.asc(path)
    case Descending => OrderBy.desc(path)
  }

  for {
    direction <- List(OrderDirection.Ascending, OrderDirection.Descending)
  } {
    implicit val implDirection: OrderDirection = direction
    List(
      builder.sortBy(_.intParam) -> "intParam",
      builder.sortBy(_.stringParam) -> "stringParam",
      builder.sortBy(_.innerClass.intParam) -> "innerClass.intParam",
      builder
        .sortBy(_.innerClassOpt.map(_.intParam)) -> "innerClassOpt.intParam",
      builder.sortBy(_.innerClass.deeper.some2) -> "innerClass.deeper.some2",
      builder
        .sortBy(_.innerClassOpt.map(_.deeperOpt).map(_.map(_.some2))) -> "innerClassOpt.deeperOpt.some2",
      builder
        .sortBy(_.innerClassOpt.flatMap(_.deeperOpt).map(_.some2)) -> "innerClassOpt.deeperOpt.some2",
      builder
        .sortBy(_.innerClass.deeper.inner.some3) -> "innerClass.deeper.inner.some3",
      builder.sortBy(
        _.innerClassOpt.flatMap(_.deeperOpt).flatMap(_.optInner).map(_.some3)
      ) -> "innerClassOpt.deeperOpt.optInner.some3"
    ).zipWithIndex.foreach {
      case ((query: QueryBuilder[QueringTestEntity], expected), idx) =>
        test("builds sortBy criteria")(query.orderCriteria).assert(
          orderCriteria => {
            orderCriteria.size == 1 &&
              orderCriteria.head == pathToOrder(direction)(expected)
          }
        )
    }

    test("builds multiple sortBy critera") {
      builder.sortBy(
        _.optionalParam.get,
        _.innerClassOpt.map(_.deeper.some2),
        _.innerClassOpt.flatMap(_.deeperOpt).get.some2
      )
    }.assert(result => {
      val expected = List(
        "optionalParam",
        "innerClassOpt.deeper.some2",
        "innerClassOpt.deeperOpt.some2"
      ).map(pathToOrder(direction))
      val passed = result.orderCriteria.toList == expected
      if (!passed) {
        println(s"""
              Expected: ${expected}
              Actual:   ${result.orderCriteria}
              """.stripMargin)
      }
      passed
    })
  }
}
object QueryBuilderSpec {
  object Model {
    case class InnerClass3(some3: Int = 3, none: Option[String] = None)
    case class InnerClass2(
        some2: Int = 2,
        inner: InnerClass3 = InnerClass3(),
        optInner: Option[InnerClass3] = None
    )

    case class InnerClass(
        intParam: Int,
        decimalParam: Double,
        optionalParam: Option[Int],
        deeper: InnerClass2 = InnerClass2(),
        deeperOpt: Option[InnerClass2] = None
    )

    case class QueringTestEntity(
        @id() id: Int,
        intParam: Int,
        stringParam: String,
        optionalParam: Option[String] = None,
        listParam: List[String] = Nil,
        innerClass: InnerClass,
        innerClassOpt: Option[InnerClass] = None
    )
  }
}
