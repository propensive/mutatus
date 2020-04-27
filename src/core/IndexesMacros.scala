package mutatus

import scala.reflect.macros._

class IndexesMacros(val c: whitebox.Context) {
  import c.universe._


  def loadIndexesImpl(
      indexes: c.Tree*
  ): c.Tree = {
    val composite = indexes.map {
        case q"mutatus.DatastoreIndex.apply[$entityType](..$properties)" =>
          val idxDef = properties.foldLeft[c.Tree](tq"_root_.mutatus.ComplexIndexDef") {
            case (tpe, q"scala.Tuple2.apply[..$_]($path, $direction)") => 
            tq"$tpe with _root_.mutatus.Property[$path, $direction]"
          }
          tq"_root_.mutatus.Index[$entityType, $idxDef]"
      }
      .reduce[c.Tree] {
        case (tq"$lhs", tq"$rhs") => tq"$lhs with $rhs"
      }
    q"""new _root_.mutatus.Schema[$composite]{}"""
  }
}
