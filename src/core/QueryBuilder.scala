package mutatus

import com.google.cloud.datastore, datastore._
import com.google.cloud.datastore, datastore.StructuredQuery.Filter
import language.experimental.macros
import scala.collection.immutable.SortedMap

case class QueryBuilder[T] private[mutatus] (
  kind: String,
  filterCriteria: Option[Filter] = None,
  orderCriteria: List[StructuredQuery.OrderBy] = Nil,
  offset: Option[Int] = None,
  limit: Option[Int] = None
) {
  import QueryBuilder._
  sealed trait OrderBy {
    def asc(path: T => Any): OrderBy
    def desc(path: T => Any): OrderBy
  }

  // Following 2 methods would not be necessary in case if we could access private members of QueryBuilder inside macro evalulation
  def withSortCriteria(orders: StructuredQuery.OrderBy*): QueryBuilder[T] = {
    copy[T](orderCriteria = orders.foldLeft(orderCriteria) {
      case (acc, order) => acc.filterNot(_.getProperty() == order.getProperty()) :+ order
    })
  }

  def withFilterCriteria(filters: StructuredQuery.Filter*): QueryBuilder[T] =
    copy(
      filterCriteria =
        Option((filterCriteria ++ filters).toList.distinct).filter(_.nonEmpty).map {
          case singleFilter :: Nil => singleFilter
          case multiple =>
            StructuredQuery.CompositeFilter.and(multiple.head, multiple.tail: _*)
        }
    )

  def where(pred: T => Boolean): QueryBuilder[T] = macro QueryBuilderMacros.whereImpl[T]
  def sortBy(pred: (T => Any)*)(
    implicit orderDirection: OrderDirection
  ): QueryBuilder[T] = macro QueryBuilderMacros.sortByImpl[T]
  def orderBy(pred: (OrderBy => Any)*): QueryBuilder[T] =
    macro QueryBuilderMacros.orderByImpl[T]

  def limit(limit: Int, offset: Int): QueryBuilder[T] = {
    this.copy(limit = Some(limit), offset = Some(offset))
  }

  /** Materializes query and returns Iterator of entities for GCP Storage */
  def find()(implicit svc: Service,
             namespace: Namespace,
             decoder: Decoder[T]): Iterator[T] = {
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

    val results = svc.read.run(query)
    new Iterator[Entity] {
      def next(): Entity = results.next()

      def hasNext: Boolean = results.hasNext
    }.map(decoder.decode(_))
  }
}

object QueryBuilder {
  sealed trait OrderDirection
  object OrderDirection {
    case object Ascending extends OrderDirection
    case object Descending extends OrderDirection
  }
}
