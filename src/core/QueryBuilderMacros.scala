package mutatus

import scala.annotation.tailrec
import scala.reflect.macros._
import mutatus.utils.BinaryTree
import mutatus.utils.BinaryTree._

class QueryBuilderMacros(val c: blackbox.Context) {
  import c.universe._
  private val self = c.prefix
  private val selectLikeOperators = Set("isEmpty",
                                        "isDefined",
                                        "last",
                                        "lastOption",
                                        "head",
                                        "headOption",
                                        "init",
                                        "tail",
                                        "get",
                                        "toString")
  private val filter = q"_root_.com.google.cloud.datastore.StructuredQuery.PropertyFilter"
  private val composite =
    q"_root_.com.google.cloud.datastore.StructuredQuery.CompositeFilter.and"
  private val operationMapping
    : PartialFunction[c.Tree, (String, Option[c.Tree]) => c.Tree] = {
    case q"==" | q"equals" =>
      (path, args) =>
        q"$filter.eq($path, ${args.get})"
    case q"<" =>
      (path, args) =>
        q"$filter.lt($path, ${args.get})"
    case q"<=" =>
      (path, args) =>
        q"$filter.le($path, ${args.get})"
    case q">" =>
      (path, args) =>
        q"$filter.gt($path, ${args.get})"
    case q">=" =>
      (path, args) =>
        q"$filter.ge($path, ${args.get})"
    case q"isEmpty" =>
      (path, args) =>
        q"$filter.isNull($path)"
    case q"isDefined" | q"nonEmpty" =>
      (path, args) =>
        q"$filter.gt($path, _root_.com.google.cloud.datastore.NullValue.of())"
    case q"contains" | q"foreach" =>
      (path, args) =>
        q"$filter.eq($path, ${args.get})"
  }

  def whereImpl[T: c.WeakTypeTag](pred: c.Expr[T => Boolean]): c.universe.Tree = {
    def buildQueryCondition(critera: AppliedCriteria): c.Tree = {
      val AppliedCriteria(path, operation, args) = critera
      operation
        .map(operationMapping(_)(path, args))
        .getOrElse(
          c.abort(
            c.enclosingPosition,
            s"mutatus: could not transform this condition to a database query"
          )
        )
    }

    CallTree(pred.tree).resolveCriteria.map(buildQueryCondition) match {
      case Nil                    => q"$self"
      case singleCondition :: Nil => q"$self.withFilterCriteria($singleCondition)"
      case multipleConditions =>
        q"$self.withFilterCriteria($composite(..$multipleConditions))"
    }
  }

  def sortByImpl[T: c.WeakTypeTag](
    pred: c.Tree*
  )(orderDirection: c.Tree): c.universe.Tree = {
    val sortBy: Seq[c.universe.Literal] = for {
      predicate <- pred
      q"(..$_) => $body" = predicate
      AppliedCriteria(path, _, _) <- CallTree(body).resolveCriteria
    } yield Literal(Constant(path))

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
    val sortBy: Seq[c.Tree] = pred
      .collect {
        case q"(..$_) => $_.asc((..$_) => $select)"  => true -> CallTree(select)
        case q"(..$_) => $_.desc((..$_) => $select)" => false -> CallTree(select)
      }
      .flatMap {
        case (isAscending, ct) =>
          ct.resolveCriteria.map {
            case AppliedCriteria(path, _, _) =>
              if (isAscending) {
                q"_root_.com.google.cloud.datastore.StructuredQuery.OrderBy.asc($path)"
              } else {
                q"_root_.com.google.cloud.datastore.StructuredQuery.OrderBy.desc($path)"
              }
          }
      }
    q"$self.withSortCriteria(..$sortBy)"
  }

  private case class CallTree(tree: mutatus.utils.BinaryTree[c.Tree]) {
    def resolveCriteria: List[AppliedCriteria] = {
      def iterate(head: mutatus.utils.BinaryTree[c.Tree],
                  prefix: String): List[AppliedCriteria] = {
        head match {
          case Node(q"&&", l, r) => iterate(l, prefix) ++ iterate(r, prefix)
          case Node(op, path, arg) if operationMapping.isDefinedAt(op) =>
            AppliedCriteria(buildPath(resolvePath(path), prefix),
                            Some(op),
                            Option(resolveArg(arg)).filterNot(_ == q"")) :: Nil
          case Node(op, path, Empty) =>
            AppliedCriteria(buildPath(resolvePath(path), prefix), Some(op), None) :: Nil
          case Node(_, path, r) => iterate(r, buildPath(resolvePath(path), prefix))
          case Leaf(v)          => AppliedCriteria(buildPath(v, prefix), None, None) :: Nil
          case Empty            => Nil
        }
      }
      iterate(tree, "")
    }

    private def buildPath(tree: c.Tree, prefix: String = ""): String = {
      //FIXME Maybe we should not use toString, as we're loosing it's context. In case of 'this' or 'super' we  may be able to get some usefull data
      val path = tree.toString().split('.').tail.mkString(".")
      (prefix, path) match {
        case ("", path)     => path
        case (prefix, "")   => prefix
        case (prefix, path) => s"$prefix.$path"
      }
    }.replaceAll("\"", "")
      .replaceAllLiterally("this.", "")
      .replaceAllLiterally("super.", "")

    private def resolveArg: mutatus.utils.BinaryTree[c.Tree] => c.Tree = {
      case Empty       => q""
      case Leaf(value) => value
      case Node(op, l, r) =>
        q"${resolveArg(l)}.${TermName(op.toString())}(${resolveArg(r)})"
    }

    private def resolvePath: mutatus.utils.BinaryTree[c.Tree] => c.Tree = {
      case Leaf(path) => path
      case Node(_, l, r) =>
        q"${buildPath(prefix = resolvePath(l).toString(), tree = resolvePath(r))}"
      case Empty => q""
    }
  }

  private object CallTree {
    def apply(tree: c.Tree): CallTree = CallTree(extract(tree))
    private def extract(tree: c.Tree): BinaryTree[c.Tree] = {
      tree match {
        case q"(..${_}) => ${body}" => extract(body)
        case q"$path.$calledMethod[$_](..$args)" =>
          Node(q"$calledMethod",
               extract(path),
               args.headOption.map(extract).getOrElse(Empty))
        case q"$path.$calledMethod(..$args)" =>
          Node(q"$calledMethod",
               extract(path),
               args.headOption.map(extract).getOrElse(Empty))
        case q"$path.${op @ TermName(name)}"
            if selectLikeOperators.contains(name) || name.head.isUpper =>
          Node(q"$op", extract(path), Empty)
        case fullPath =>
          selectLikeOperators.find(fullPath.toString().contains) match {
            case None => Leaf(fullPath)
            case Some(operator) =>
              val q"${lhs}.${operator}.${rhs}" = fullPath
              Node(q"$operator", extract(lhs), extract(q"x.$rhs")) //x was add rhs to avoid removing it while building path as it is valid select
          }
      }
    }
  }

  private case class AppliedCriteria(path: String,
                                     operation: Option[c.Tree],
                                     arg: Option[c.Tree])
}
