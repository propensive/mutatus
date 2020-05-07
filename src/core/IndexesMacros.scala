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
      q"mutatus.DatastoreIndex.apply[$entityType](..$properties)" = index
      criterias = for {
        property <- properties
        (selector, order) = property match {
          case q"mutatus.PropertySelector.Asc.apply[$_]($selector)"  => selector -> AscendingOrder
          case q"mutatus.PropertySelector.Desc.apply[$_]($selector)" => selector -> DescendingOrder
          }
        criteria <- CallTree(selector).resolveCriteria
      } yield criteria -> order
    } yield entityType -> criterias

    val indexesTpe = for {
      (entityType, criteria) <- indexesCriteria
      indexDef = criteria.foldLeft[c.Tree](tq"_root_.mutatus.ComplexIndexDef") {
        case (tpe, (criteria, order)) => tq"$tpe with _root_.mutatus.Property[${criteria.path}, $order]"
      }
    } yield tq"_root_.mutatus.Index[$entityType, $indexDef]"

    val compositeTpe = indexesTpe.reduce[c.Tree] {
        case (tq"$lhs", tq"$rhs") => tq"$lhs with $rhs"
      }

    q"""new _root_.mutatus.Schema[$compositeTpe]{}"""
  }

}
