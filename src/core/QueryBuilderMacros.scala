package mutatus

import scala.annotation.tailrec
import scala.reflect.macros._
import mutatus.utils.BinaryTree
import mutatus.utils.BinaryTree._
import java.io.FileWriter
import scala.collection.immutable.Nil
import io.opencensus.common.ServerStatsFieldEnums.Id
import scala.reflect.api.Scopes

class QueryBuilderMacros(val c: whitebox.Context) {
  import c.universe._
  private val self = c.prefix

  private val selectLikeOperators = Set(
    "isEmpty",
    "isDefined",
    "last",
    "lastOption",
    "head",
    "headOption",
    "init",
    "tail",
    "get",
    "toString"
  )
  private val filter =
    q"_root_.com.google.cloud.datastore.StructuredQuery.PropertyFilter"
  private val composite =
    q"_root_.com.google.cloud.datastore.StructuredQuery.CompositeFilter.and"
  private val orderBy =
    q"_root_.com.google.cloud.datastore.StructuredQuery.OrderBy"

  private val operationMapping
      : PartialFunction[c.Tree, (String, Option[c.Tree]) => c.Tree] = {
    case q"==" | q"equals" => (path, args) => q"$filter.eq($path, ${args.get})"
    case q"<"              => (path, args) => q"$filter.lt($path, ${args.get})"
    case q"<="             => (path, args) => q"$filter.le($path, ${args.get})"
    case q">"              => (path, args) => q"$filter.gt($path, ${args.get})"
    case q">="             => (path, args) => q"$filter.ge($path, ${args.get})"
    case q"isEmpty"        => (path, args) => q"$filter.isNull($path)"
    case q"isDefined" | q"nonEmpty" =>
      (path, args) =>
        q"$filter.gt($path, _root_.com.google.cloud.datastore.NullValue.of())"
    case q"contains" | q"foreach" =>
      (path, args) => q"$filter.eq($path, ${args.get})"
  }

  def filterImpl[T: c.WeakTypeTag](
      pred: c.Expr[T => Boolean]
  ): c.universe.Tree = {
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
    val criteria = CallTree(pred.tree).resolveCriteria
    val newFilters = criteria.map(buildQueryCondition) match {
      case Nil           => None
      case single :: Nil => Some(single)
      case multiple      => Some(q"$composite(..$multiple)")
    }

    val idxDef: CompoundTypeTree =
      self.actualType.member("IdxDef": TypeName).info match {
        case tpe: CompoundTypeTree => tpe
        case tpe                   => tq"$tpe with _root_.mutatus.IndexDef"
      }

    val newIdxDef = criteria
      .foldLeft(idxDef) {
        case (tpe, c) =>
          tq"$tpe with _root_.mutatus.Property[${q"${c.path}"}, _root_.mutatus.OrderDirection]"
      }

    newFilters
      .map { newFilter =>
        q"""{
          val current = $self
          new QueryBuilder[${weakTypeOf[T]}](
            current.kind,
            current.query.copy(
            filterCriteria = current.filterCriteria
              .map($composite(_, $newFilter))
              .orElse(Some($newFilter))
            )
          ){
            type IdxDef = ${newIdxDef}
          }
        }"""
      }
      .getOrElse(q"$self")
  }

  def loadIndexesImpl(
      indexes: c.Tree*
  ): c.Tree = {
    val composite = indexes.map {
        case q"mutatus.DatastoreIndex.apply[$entityType](..$properties)" =>
          val idxDef = properties.foldLeft(tq"_root_.mutatus.IndexDef with _root_.scala.Singleton") {
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

  def sortByImpl[T: c.WeakTypeTag](pred: c.Tree*): c.universe.Tree = {

    val criteria = pred.flatMap(CallTree(_).resolveCriteria)
    val sortBy: Seq[c.Tree] = criteria
      .map {
        case AppliedCriteria(path, _, _) => q"$orderBy.asc($path)"
      }

    val idxDef: CompoundTypeTree =
      self.actualType.member("IdxDef": TypeName).info match {
        case tpe: CompoundTypeTree => tpe
        case tpe                   => tq"$tpe with _root_.mutatus.IndexDef"
      }

    val newIdxDef = criteria
      .foldLeft(idxDef) {
        case (tpe, ac) =>
          tq"$tpe with _root_.mutatus.Property[${q"${ac.path}"}, _root_.mutatus.OrderDirection.Ascending.type]"
      }

    q"""{
      val current = $self
      val newOrder = List(..$sortBy).foldLeft(current.orderCriteria){
        case (acc, orderBy) => orderBy :: acc.filterNot(_.getProperty() == orderBy.getProperty()) 
      }

      new QueryBuilder[${weakTypeOf[T]}](current.kind, current.query.copy(orderCriteria = newOrder)){
        type IdxDef = $newIdxDef
      }
    }
    """
  }

  def reverseImpl[T: c.WeakTypeTag]: c.universe.Tree = {
    import compat._

    val symbolAscending = symbolOf[mutatus.OrderDirection.Ascending.type]
    val symbolDescedning = symbolOf[mutatus.OrderDirection.Descending.type]

    val tpe = self.actualType
      .member("IdxDef": TypeName)
      .typeSignatureIn(self.actualType)
      .normalize
    val newIdxDef = tpe match {
      case RefinedType(parents, scope) =>
        val fixed = parents.collect {
          case original @ TypeRef(pre, sym, path :: orderType :: Nil) =>
            show(orderType) match {
              case "mutatus.OrderDirection.Ascending.type" =>
                TypeRef(pre, sym, path :: symbolDescedning.toType :: Nil)
              case "mutatus.OrderDirection.Descending.type" =>
                TypeRef(pre, sym, path :: symbolAscending.toType :: Nil)
              case other => original
            }
          case other => other
        }
        tq"${RefinedType(fixed, scope)}"
    }

    q"""{
      val current = $self
      val reversedOrder = current.query.orderCriteria.map { critieria =>
        val newOrderFn = critieria.getDirection() match {
          case $orderBy.Direction.ASCENDING  => $orderBy.desc _
          case $orderBy.Direction.DESCENDING => $orderBy.asc _
        }
        newOrderFn(critieria.getProperty)
      }
      new QueryBuilder[${weakTypeOf[T]}](current.kind, current.query.copy(orderCriteria = reversedOrder)) {
        type IdxDef = ${newIdxDef}
      }
  }
  """
  }

  private case class CallTree(tree: mutatus.utils.BinaryTree[c.Tree]) {
    def resolveCriteria: List[AppliedCriteria] = {
      def iterate(
          head: mutatus.utils.BinaryTree[c.Tree],
          prefix: String
      ): List[AppliedCriteria] = {
        head match {
          case Node(q"&&", l, r) => iterate(l, prefix) ++ iterate(r, prefix)
          case Node(op, path, arg) if operationMapping.isDefinedAt(op) =>
            AppliedCriteria(
              buildPath(resolvePath(path), prefix),
              Some(op),
              Option(resolveArg(arg)).filterNot(_ == q"")
            ) :: Nil
          case Node(op, path, Empty) =>
            AppliedCriteria(
              buildPath(resolvePath(path), prefix),
              Some(op),
              None
            ) :: Nil
          case Node(_, path, r) =>
            iterate(r, buildPath(resolvePath(path), prefix))
          case Leaf(v) =>
            AppliedCriteria(buildPath(v, prefix), None, None) :: Nil
          case Empty => Nil
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
    def apply(tree: c.Tree): CallTree = tree match {
      case q"(..$_) => $body" => CallTree(extract(body))
      case _ =>
        c.abort(
          c.enclosingPosition,
          "mutatus: Critieria must be passed as lambda expresion, passing values is prohibited"
        )
    }
    private def extract(tree: c.Tree): BinaryTree[c.Tree] = {
      tree match {
        case q"(..${_}) => ${body}" => extract(body)
        case q"$path.$calledMethod[$_](..$args)" =>
          Node(
            q"$calledMethod",
            extract(path),
            args.headOption.map(extract).getOrElse(Empty)
          )
        case q"$path.$calledMethod(..$args)" =>
          Node(
            q"$calledMethod",
            extract(path),
            args.headOption.map(extract).getOrElse(Empty)
          )
        case q"$path.${op @ TermName(name)}"
            if selectLikeOperators.contains(name) || name.head.isUpper =>
          Node(q"$op", extract(path), Empty)
        case fullPath =>
          selectLikeOperators.find(fullPath.toString().contains) match {
            case None => Leaf(fullPath)
            case Some(operator) =>
              val q"${lhs}.${operator}.${rhs}" = fullPath
              Node(
                q"$operator",
                extract(lhs),
                extract(
                  q"x.$rhs"
                ) //x was add rhs to avoid removing it while building path as it is valid select
              )
          }
      }
    }
  }

  private case class AppliedCriteria(
      path: String,
      operation: Option[c.Tree],
      arg: Option[c.Tree]
  )
}
