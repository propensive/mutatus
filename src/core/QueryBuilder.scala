package mutatus

import com.google.cloud.datastore, datastore._
import com.google.cloud.datastore, datastore.StructuredQuery.Filter
import language.experimental.macros
import scala.collection.immutable.SortedMap
import com.google.cloud.datastore.StructuredQuery.OrderBy.Direction
import com.google.cloud.datastore.StructuredQuery.OrderBy
import quarantine._

case class QueryBuilder[T] private[mutatus] (
    kind: String,
    filterCriteria: Option[Filter] = None,
    orderCriteria: List[StructuredQuery.OrderBy] = Nil,
    offset: Option[Int] = None,
    limit: Option[Int] = None
) {
  // Following 2 methods would not be necessary in case if we could access private members of QueryBuilder inside macro evalulation
  def withSortCriteria(orders: StructuredQuery.OrderBy*): QueryBuilder[T] = {
    copy[T](orderCriteria = orders.foldLeft(orderCriteria) {
      case (acc, order) =>
        order :: acc.filterNot(_.getProperty() == order.getProperty())
    })
  }

  def withFilterCriteria(filters: StructuredQuery.Filter*): QueryBuilder[T] =
    copy[T](
      filterCriteria = Option((filterCriteria ++ filters).toList.distinct)
        .filter(_.nonEmpty)
        .map {
          case singleFilter :: Nil => singleFilter
          case multiple =>
            StructuredQuery.CompositeFilter
              .and(multiple.head, multiple.tail: _*)
        }
    )

  def filter(pred: T => Boolean): QueryBuilder[T] =
    macro QueryBuilderMacros.filterImpl[T]
  def sortBy(pred: (T => Any)*): QueryBuilder[T] =
    macro QueryBuilderMacros.sortByImpl[T]

  def reverse: QueryBuilder[T] = {
    val reversedOrder = orderCriteria.map { critieria =>
      val newOrderFn = critieria.getDirection() match {
        case Direction.ASCENDING  => OrderBy.desc _
        case Direction.DESCENDING => OrderBy.asc _
      }
      newOrderFn(critieria.getProperty)
    }
    copy[T](orderCriteria = reversedOrder)
  }
  def take(n: Int): QueryBuilder[T] = copy[T](limit = Some(n))
  def drop(n: Int): QueryBuilder[T] = copy[T](offset = Some(n))
  def slice(offset: Int, limit: Int) =
    copy[T](offset = Some(offset), limit = Some(limit))

  /** Materializes query and returns Stream of entities for GCP Storage */
  def run()(
      implicit ctx: Context with Context.ReadApi,
      namespace: Namespace,
      decoder: Decoder[T]
  ): Stream[mutatus.Result[T]] = {
    val baseQuery = namespace.option.foldLeft(
      Query.newEntityQueryBuilder().setKind(kind)
    )(_.setNamespace(_))
    val filtered = filterCriteria.foldLeft(baseQuery)(_.setFilter(_))
    val ordered = orderCriteria.headOption.foldLeft(filtered)(
      _.setOrderBy(_, orderCriteria.tail: _*)
    )
    val limited = limit.foldLeft(ordered)(_.setLimit(_))
    val withOffset = offset.foldLeft(limited)(_.setOffset(_))
    val query = withOffset.build()

    Result {
      val results = ctx.read.run(query)
      new Iterator[Entity] {
        def next(): Entity = results.next()
        def hasNext: Boolean = results.hasNext
      }.toStream.map(decoder.decode)
    }.extenuate {
        case exc: DatastoreException => DatabaseException(exc)
      } match {
      case Answer(entities) => entities
      case Error(error)     => Stream(Error(error))
      case Surprise(error)  => Stream(Surprise(error))
    }
  }
}
