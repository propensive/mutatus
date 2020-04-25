package mutatus

import com.google.cloud.datastore, datastore._
import com.google.cloud.datastore, datastore.StructuredQuery.Filter
import language.experimental.macros
import scala.collection.immutable.SortedMap
import com.google.cloud.datastore.StructuredQuery.OrderBy.Direction
import com.google.cloud.datastore.StructuredQuery.OrderBy
import quarantine._
import scala.reflect.runtime.universe.WeakTypeTag
import io.opencensus.common.ServerStatsFieldEnums.Id
import scala.annotation.implicitNotFound

case class Query(
    filterCriteria: Option[Filter] = None,
    orderCriteria: List[StructuredQuery.OrderBy] = Nil,
    offset: Option[Int] = None,
    limit: Option[Int] = None
)

class QueryBuilder[T: WeakTypeTag](
    val kind: String,
    val query: Query = Query()
) extends {
  type IdxDef <: mutatus.IndexDef
  final type Idx = Index[T, IdxDef]
  val self = this

  def filterCriteria: Option[Filter] = query.filterCriteria
  def orderCriteria: List[StructuredQuery.OrderBy] = query.orderCriteria
  def offset: Option[Int] = query.offset
  def limit: Option[Int] = query.limit

  def filter(pred: T => Boolean): QueryBuilder[T] =
    macro QueryBuilderMacros.filterImpl[T]
  def sortBy(pred: (T => Any)*): QueryBuilder[T] =
    macro QueryBuilderMacros.sortByImpl[T]

  def reverse: QueryBuilder[T] = macro QueryBuilderMacros.reverseImpl[T]

  def take(n: Int) = new QueryBuilder[T](kind, query.copy(limit = Some(n))) {
    type IdxDef = self.IdxDef
  }
  def drop(n: Int) = new QueryBuilder[T](kind, query.copy(offset = Some(n))) {
    type IdxDef = self.IdxDef
  }

  def slice(offset: Int, limit: Int) = drop(offset).take(limit)

  /** Materializes query and returns Stream of entities for GCP Storage */
  def run()(
      implicit svc: Service = Service.default,
      namespace: Namespace,
      decoder: Decoder[T],
      ev: Schema[Idx]
  ): mutatus.Result[Stream[mutatus.Result[T]]] = {
    val baseQuery = namespace.option.foldLeft(
      datastore.Query.newEntityQueryBuilder().setKind(kind)
    )(_.setNamespace(_))
    val filtered = query.filterCriteria.foldLeft(baseQuery)(_.setFilter(_))
    val ordered = query.orderCriteria.headOption.foldLeft(filtered)(
      _.setOrderBy(_, query.orderCriteria.tail: _*)
    )
    val limited = query.limit.foldLeft(ordered)(_.setLimit(_))
    val withOffset = query.offset.foldLeft(limited)(_.setOffset(_))
    val finalQuery = withOffset.build()

    for {
      results <- mutatus.Result(svc.read.run(finalQuery))
      entities = new Iterator[Entity] {
        def next(): Entity = results.next()

        def hasNext: Boolean = results.hasNext
      }.toStream
    } yield entities.map(decoder.decode)
  }.extenuate {
    case exc: DatastoreException => DatabaseException(exc)
  }
}
