package mutatus

import com.google.cloud.datastore, datastore._
import com.google.cloud.datastore, datastore.StructuredQuery.Filter
import language.experimental.macros
import quarantine._
import scala.reflect.runtime.universe.WeakTypeTag
import Mutatus._

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
  /** Final IndexDef used when resolving it's existance within Schema */
  type IdxDef <: mutatus.Schema.IndexType
  
  /** IndexDef containing all informations properties used in query. Used only internally within macros.*/
  type FullIdxDef <: mutatus.Schema.IndexType
  final type Idx = Schema.Index[T, IdxDef]
  
  final val self = this

  def filterCriteria: Option[Filter] = query.filterCriteria
  def orderCriteria: List[StructuredQuery.OrderBy] = query.orderCriteria
  def offset: Option[Int] = query.offset
  def limit: Option[Int] = query.limit

  def filter(pred: T => Boolean): QueryBuilder[T] = macro QueryBuilderMacros.filterImpl[T]
  def sortBy(pred: (T => Any)*): QueryBuilder[T] = macro QueryBuilderMacros.sortByImpl[T]
  def reverse: QueryBuilder[T] = macro QueryBuilderMacros.reverseImpl[T]
  def take(limit: Int): QueryBuilder[T] = macro QueryBuilderMacros.takeImpl[T]
  def drop(offset: Int): QueryBuilder[T] = macro QueryBuilderMacros.dropImpl[T] 
  def slice(offset: Int, limit: Int): QueryBuilder[T] = macro QueryBuilderMacros.sliceImpl[T]

  def build(implicit ctx: Context with Context.ReadApi,
      namespace: Namespace) = {
        val baseQuery = namespace.option.foldLeft(
          datastore.Query.newEntityQueryBuilder().setKind(kind)
        )(_.setNamespace(_))
        val filtered = query.filterCriteria.foldLeft(baseQuery)(_.setFilter(_))
        val ordered = query.orderCriteria.headOption.foldLeft(filtered)(
          _.setOrderBy(_, query.orderCriteria.tail: _*)
        )
        val limited = query.limit.foldLeft(ordered)(_.setLimit(_))
        val withOffset = query.offset.foldLeft(limited)(_.setOffset(_))
        withOffset.build()
      }

  /** Materializes query and returns Stream of entities for GCP Storage */
  def runUnsafe()(
      implicit ctx: Context with Context.ReadApi,
      namespace: Namespace,
      decoder: Decoder[T]
  ): Result[Stream[Result[T]]] = {
      for {
        results <- Result(ctx.read.run(build))
        entities = new Iterator[Entity] {
          def next(): Entity = results.next()

          def hasNext: Boolean = results.hasNext
        }.toStream
      } yield entities.map(decoder.decode)
    }.extenuate {
      case exc: DatastoreException => DatabaseException(exc)
    }

  /**
   * Materializes query and returns results Stream of entities for GCP Storage 
   * Run query with validation that schema contains index definition which contains properties in correct order without redundancy. 
   * Such validations are needed to unsure that query may be executed by Datastore
   */
  def run()(
      implicit ctx: Context with Context.ReadApi,
      namespace: Namespace,
      decoder: Decoder[T],
      ev: Schema[Idx]
  ): Result[Stream[Result[T]]] = macro QueryBuilderMacros.runImpl[T]
}
