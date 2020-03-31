package mutatus

import scala.annotation.tailrec
import scala.reflect.macros._

class QueryBuilderMacros(val c: blackbox.Context) {
  import c.universe._
  private val self = c.prefix
  private val selectLikeOperators = Set(
    "isEmpty", "isDefined",
    "last", "lastOption", "head", "headOption", "init", "tail", "get",
    "toString")

  def whereImpl[T: c.WeakTypeTag](pred: c.Expr[T => Boolean]): c.universe.Tree = {
    val filter    = q"_root_.com.google.cloud.datastore.StructuredQuery.PropertyFilter"
    val composite = q"_root_.com.google.cloud.datastore.StructuredQuery.CompositeFilter.and"
    val predicates = pred match {
      case Expr(q"(..$_) => $body") => body
    }

    @scala.annotation.tailrec
    def splitFilterCriteria(tree: c.Tree, resolved: List[c.Tree] = Nil): List[c.Tree] = {
      tree match {
        case q"$_ || $_" => c.abort(c.enclosingPosition, s"Google Query Language does not support OR operator: ${show(tree)}")
        case q"$head && $last" =>
          splitFilterCriteria(head.asInstanceOf[c.Tree], last :: resolved)
        case q"$head" => head :: resolved
      }
    }

    def buildQueryCondition(condition: c.Tree): c.Tree = {
      val DisassembledTree(path, operations) = disassembleSelectTree(condition)
      val operationsMapping: PartialFunction[c.TermName, List[c.Tree] => c.Tree] = {
        case TermName("$eq$eq")      => args => q"$filter.eq($path, ${args.head})"
        case TermName("$less")       => args => q"$filter.lt($path, ${args.head})"
        case TermName("$less$eq")    => args => q"$filter.le($path, ${args.head})"
        case TermName("$greater")    => args => q"$filter.gt($path, ${args.head})"
        case TermName("$greater$eq") => args => q"$filter.ge($path, ${args.head})"
        case TermName("isEmpty")     => _    => q"$filter.isNull($path)"

        case TermName("isDefined") | TermName("nonEmpty") =>
          _ => q"$filter.gt($path, _root_.com.google.cloud.datastore.NullValue.of())"
        case TermName("contains") | TermName("foreach") =>
          args => q"$filter.eq($path, ${args.head})"
      }

      operations.collectFirst{
        case (op, arg) if operationsMapping.isDefinedAt(op) =>
          operationsMapping(op)(arg)
      }.getOrElse(c.abort(c.enclosingPosition, s"Not found matching operation mapping for any of ${operations.map(_._1)}"))
    }

    val query = splitFilterCriteria(predicates)
      .map(buildQueryCondition) match {
      case Nil => c.abort(c.enclosingPosition, "Generated empty query, aborting")
      case singleCondition :: Nil => singleCondition
      case multipleConditions =>       q"$composite(..$multipleConditions)"
    }
    q"$self.withFilterCriteria($query)"
  }

  def sortByImpl[T: c.WeakTypeTag](pred: c.Tree*)(orderDirection: c.Tree): c.universe.Tree = {
    val sortBy: Seq[c.universe.Literal] = pred
      .collect{
        case q"(..$_) => $body" => disassembleSelectTree(body)
      }
      .map(_.pathLiteral)

    q"""{
         val isAscending = $orderDirection == _root_.mutatus.QueryBuilder.OrderDirection.Ascending
         val sortBy: List[StructuredQuery.OrderBy] = if(isAscending){
            List(..$sortBy).map(_root_.com.google.cloud.datastore.StructuredQuery.OrderBy.asc(_))
          }else{
            List(..$sortBy).map(_root_.com.google.cloud.datastore.StructuredQuery.OrderBy.desc(_))
          }
          $self.withSortCriteria(sortBy:_*)
        }
       """
  }

  def orderByImpl[T: c.WeakTypeTag](pred: c.Tree*): c.universe.Tree = {
    val conditions = pred.collect {
      case q"(..$_) => $_.asc((..$_) => $select)" =>   true -> disassembleSelectTree(select)
      case q"(..$_) => $_.desc((..$_) => $select)" =>  false -> disassembleSelectTree(select)
    }

    val sortBy = conditions.map { case (isAscending, dt) =>
      if (isAscending) {
        q"_root_.com.google.cloud.datastore.StructuredQuery.OrderBy.asc(${dt.pathLiteral})"
      } else {
        q"_root_.com.google.cloud.datastore.StructuredQuery.OrderBy.desc(${dt.pathLiteral})"
      }
    }
    q"$self.withSortCriteria(..$sortBy)"
  }

  type OpArgs = (c.TermName, List[c.Tree])
  private case class DisassembledTree(path: String, operations: List[OpArgs]){
    val pathLiteral: c.universe.Literal = Literal(Constant(path))
  }

  private def disassembleSelectTree(tree: c.Tree): DisassembledTree = {
    @tailrec
    def dissembleSelectTree(
        tree: c.Tree,
        selects: List[c.TermName] = Nil,
        applies: List[OpArgs] = Nil): (String, List[OpArgs]) = {
      def isSubpath(op: String): Boolean = {
        selectLikeOperators.contains(op) || op.headOption.exists(_.isUpper)
      }

      tree match {
        case q"$select.${operator}[$_](..$args)"         => dissembleSelectTree(select, selects, (operator, args) :: applies)
        case q"$select.${operator}(..$args)"             => dissembleSelectTree(select, selects, (operator, args) :: applies)
        case q"$select.${TermName(op)}" if isSubpath(op) => dissembleSelectTree(select, selects, (TermName(op), Nil) :: applies)
        case q"$select.${operator}"                      => dissembleSelectTree(select, operator :: selects, applies)
        case _                                           => selects.collect{case TermName(name) => name}.mkString(".") -> applies
      }
    }

    @tailrec
    def disassembleTree(
          path: String,
          tree: c.Tree,
          nextOperations: List[OpArgs],
          allOperations: List[OpArgs]): DisassembledTree = {
      val (subPath, innerOperation) = dissembleSelectTree(tree)
      val newPath = (path, subPath) match {
        case (path, "") => path
        case ("", subPath) => subPath
        case (path, subPath) => s"$path.$subPath"
      }
      (innerOperation ++ nextOperations) match {
        case (op, fn @ q"(..$_) => $body" :: Nil) :: next => disassembleTree(newPath, body, next, (op, fn) :: allOperations)
        case (opArgs @ (_, select :: Nil)) :: next        => disassembleTree(newPath, select, next, opArgs :: allOperations)
        case (opArgs @ (_, Nil)) :: next                  => disassembleTree(newPath, q"", next, opArgs :: allOperations)
        case t :: Nil                                     => DisassembledTree(newPath, t :: allOperations)
        case Nil                                          => DisassembledTree(newPath, allOperations)
      }
    }
    disassembleTree("", tree, Nil, Nil)
  }
}