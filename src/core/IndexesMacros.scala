package mutatus

import scala.reflect.macros._
import mutatus.utils.MacroHelpers

class IndexesMacros(val c: whitebox.Context) extends MacroHelpers {
  import c.universe._

  def extractSchemaImpl(
      indexes: c.Tree*
  ): c.Tree = {

    val indexesCriteria = for {
      index <- indexes
      q"mutatus.Index.apply[$entityType](..$properties)" = index
      criterias = for {
        property <- properties
        (selector, order) = property match {
          case q"mutatus.Asc.apply[$_]($selector)"  => selector -> AscendingOrder
          case q"mutatus.Desc.apply[$_]($selector)" => selector -> DescendingOrder
        }
        criteria <- CallTree(selector).resolveCriteria
      } yield criteria -> order
    } yield (entityType, criterias)

    val indexesTpe = for {
      (entityType, criteria) <- indexesCriteria
      indexDef = criteria.foldLeft[c.Tree](tq"_root_.mutatus.Schema.IndexType.Complex") {
        case (tpe, (criteria, order)) => tq"$tpe with _root_.mutatus.Schema.Property[${criteria.path}, $order]"
      }
    } yield tq"_root_.mutatus.Schema.Index[$entityType, $indexDef]"

    val compositeTpe = indexesTpe.reduce[c.Tree] {
      case (tq"$lhs", tq"$rhs") => tq"$lhs with $rhs"
    }

    val indexDefs = for{
      (entityType, criteria) <- indexesCriteria
      indexProperties = for{
         (ac, orderTpe) <- criteria
          order = if(orderTpe.equalsStructure(AscendingOrder)) q"_root_.mutatus.Schema.OrderDirection.Ascending"
          else q"_root_.mutatus.Schema.OrderDirection.Descending"
      } yield q"new _root_.mutatus.SchemaDef.Property(${ac.path}, $order)"
      indexDef = q"""{
        val meta = implicitly[_root_.adversaria.TypeMetadata[$entityType]]
        new _root_.mutatus.SchemaDef.IndexDef[$entityType](meta.typeName, ..$indexProperties)
      }"""
    } yield indexDef

    q"""new _root_.mutatus.SchemaDef[$compositeTpe](..$indexDefs)"""
  }

}
