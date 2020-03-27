package mutatus

import scala.reflect.macros._

object QueryBuilderMacros {
  def whereImpl[T: c.WeakTypeTag](c: blackbox.Context)(pred: c.Expr[T => Boolean]): c.universe.Tree = {
    import c.universe._
    val self = c.prefix
    val predicates = pred match {
      case Expr(q"(..$_) => $body") => body
    }

    @scala.annotation.tailrec
    def resolveConditions(tree: c.Tree, resolved: List[c.Tree] = Nil): List[c.Tree] = {
      tree match {
        case q"$_ || $_" => c.abort(c.enclosingPosition, s"Google Query Language does not support OR operator: ${show(tree)}")
        case q"$head && $last" =>
          resolveConditions(head.asInstanceOf[c.Tree], last :: resolved)
        case q"$head" => head :: resolved
      }
    }

    val filter    = q"_root_.com.google.cloud.datastore.StructuredQuery.PropertyFilter"
    val composite = q"_root_.com.google.cloud.datastore.StructuredQuery.CompositeFilter.and"
    val queries: List[c.Tree] = resolveConditions(predicates.asInstanceOf[c.Tree])
      .map {
        case q"$_.${field}.${operator}[$_](..$args)" => (field, operator, args)
        case q"$_.${field}.${operator}(..$args)" => (field, operator, args)
        case q"$_.${field}.${operator}" => (field, operator, Nil)
      }.map {
      case (field, operator, args) =>
        val fieldName = field match {
          case TermName(fieldName) => Literal(Constant(fieldName))
        }

        operator match {
          case TermName("$eq$eq") => q"$filter.eq($fieldName, ${args.head})"
          case TermName("$less") => q"$filter.lt($fieldName, ${args.head})"
          case TermName("$less$eq") => q"$filter.le($fieldName, ${args.head})"
          case TermName("$greater") => q"$filter.gt($fieldName, ${args.head})"
          case TermName("$greater$eq") => q"$filter.ge($fieldName, ${args.head})"
          case TermName("isEmpty") => q"$filter.isNull($fieldName)"
          case TermName("contains") => q"$filter.eq($fieldName, ${args.head})"
          case other =>
            c.abort(c.enclosingPosition, s"Not defined handling for operator $other and args $args")
        }
    }

    val query = if (queries.size > 1) {
      q"$composite(..$queries)"
    } else {
      queries.head
    }
    q"$self.withFilterCriteria($query)"
  }

  def sortByImpl[T: c.WeakTypeTag](c: blackbox.Context)(pred: c.Tree*)(orderDirection: c.Tree): c.universe.Tree = {
    import c.universe._
    val self = c.prefix
    val sortBy: Seq[Literal] = pred.collect {
      case q"(..$_) => $_.${field}" =>
        field match {
          case TermName(fieldname) => val fieldLiteral = Literal(Constant(fieldname))
            fieldLiteral
        }
    }

    q""" {
         val isAscending = $orderDirection == _root_.mutatus.OrderDirection.Ascending
         val sortBy: List[StructuredQuery.OrderBy] = if(isAscending){
            List(..$sortBy).map(_root_.com.google.cloud.datastore.StructuredQuery.OrderBy.asc(_))
          }else{
            List(..$sortBy).map(_root_.com.google.cloud.datastore.StructuredQuery.OrderBy.desc(_))
          }

        $self.withSortCriteria(sortBy:_*)
        }
       """
  }
  
  def orderByImpl[T: c.WeakTypeTag](c: blackbox.Context)(pred: c.Tree*): c.universe.Tree = {
    import c.universe._

    val self = c.prefix
    val sortBy: Seq[c.Tree] = pred.collect {
      case q"(..$_) => $_.asc((..$_) => $_.${field})" =>
        field match {
          case TermName(fieldname) => val fieldLiteral = Literal(Constant(fieldname))
            q"_root_.com.google.cloud.datastore.StructuredQuery.OrderBy.asc($fieldLiteral)"
        }
      case q"(..$_) => $_.desc((..$_) => $_.${field})" =>
        field match {
          case TermName(fieldname) => val fieldLiteral = Literal(Constant(fieldname))
            q"_root_.com.google.cloud.datastore.StructuredQuery.OrderBy.desc($fieldLiteral)"
        }
    }
    q"$self.withSortCriteria(..$sortBy)"
  }
}