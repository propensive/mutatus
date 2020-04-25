package mutatus

import scala.reflect.runtime.universe.WeakTypeTag
import language.experimental.macros

sealed trait OrderDirection
object OrderDirection {
  // sealed trait AnyOrder extends OrderDirection
  case object Ascending extends OrderDirection
  case object Descending extends OrderDirection
}

trait Schema[+T <: Index[_, _]]
object Schema{
  def load(indexes: DatastoreIndex[_]*): Schema[_ <: Index[_, _]] =  macro QueryBuilderMacros.loadIndexesImpl
}
trait IndexDef
trait Index[Entity, +Properies <: IndexDef]
trait Property[P <: Singleton, +OrderDirection]

case class DatastoreIndex[T](properties: (String,OrderDirection)*)
