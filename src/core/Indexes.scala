package mutatus

import scala.reflect.runtime.universe.WeakTypeTag
import language.experimental.macros
import adversaria.TypeMetadata

/** Contains type defininitions of all Entity indexes */
sealed trait Schema[+T <: Schema.Index[_, _]]
object Schema {
  implicit def simpleIndexSchema[T]: Schema[Index[T, IndexType.Simple]] = new Schema[Index[T, IndexType.Simple]] {}

  /** Base type for each Index Property definition.
    * IndexDef is used within QueryBuilder to check is query can be executed with predefined Datastore Indexes
    * Each IndexDef may be extended with Property
    */
  sealed trait IndexType
  object IndexType {

    /** Represents index which needs defined Complex/Manual Index inside Datastore to be corectlly evaluated */
    trait Complex extends IndexType

    /** Represents index which does not need manually created Index within Datastore. Such query is executed using built-in Indexes*/
    trait Simple extends IndexType
  }

  trait Index[Entity, +Properies <: IndexType]

  /** Property defines single Index entry for given path P, defined as literal singleton and OrderDirection in which it is indexed inside Datastore  */
  trait Property[P <: Singleton, +Order <: OrderDirection] { _: IndexDef => }

  /** Internal trait used to mark property set to be me the most significent position. Used for internal checks and query validations */
  trait MostSignificentSortOrder

  /** Internal trait used to mark that given query property would be used within filter crtieria*/
  trait Filtered
  trait InequalityFiltered extends Filtered
  trait EqualityFiltered extends Filtered

  /** Order direction for given Index Proprty existing in schema */
  sealed trait OrderDirection

  object OrderDirection {
    case object Ascending extends OrderDirection
    case object Descending extends OrderDirection
  }
}

case class SchemaDef[+T <: Schema.Index[_, _]](indexes: SchemaDef.IndexDef[_]*) {
  def using[R](fn: Schema[T] => Result[R]) = {
    val schema = new Schema[T] {} //TODO Validations that make indexes exists in datastore
    fn(schema)
  }
}

object SchemaDef {
  case class IndexDef[+T](kind: String, properties: Property*)
  case class Property(path: String, ordering: Schema.OrderDirection)
  def apply[T](indexes: Index[T]*): SchemaDef[_] = macro IndexesMacros.extractSchemaImpl
}

case class Index[-T](properties: PropertySelector[T]*)
sealed abstract class PropertySelector[-T](val orderDirection: Schema.OrderDirection) {
  def selector: T => Any
}
case class Asc[-T](selector: T => Any) extends PropertySelector[T](Schema.OrderDirection.Ascending)
case class Desc[-T](selector: T => Any) extends PropertySelector[T](Schema.OrderDirection.Descending)
