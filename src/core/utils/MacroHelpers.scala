package mutatus.utils

import scala.reflect.macros.whitebox
import mutatus.utils.BinaryTree._

trait MacroHelpers {
  val c: whitebox.Context
  import c.universe._

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
  
  final val filter =    q"_root_.com.google.cloud.datastore.StructuredQuery.PropertyFilter"
  final val composite = q"_root_.com.google.cloud.datastore.StructuredQuery.CompositeFilter.and"
  final val orderBy =   q"_root_.com.google.cloud.datastore.StructuredQuery.OrderBy"

  final val Property =           tq"_root_.mutatus.Property"
  final val AscendingOrder =     tq"_root_.mutatus.OrderDirection.Ascending.type"
  final val DescendingOrder =    tq"_root_.mutatus.OrderDirection.Descending.type"
  final val InequalityFiltered = tq"_root_.mutatus.InequalityFiltered"
  final val EqualityFiltered =   tq"_root_.mutatus.EqualityFiltered"
  final val MSSortOrder =        tq"_root_.mutatus.MostSignificentSortOrder"

  protected val equalityOperators: Set[c.Tree] = Set(q"==", q"equals", q"isEmpty", q"contains", q"foreach")
  protected val operationMapping: PartialFunction[c.Tree, (String, Option[c.Tree]) => c.Tree] = {
    case q"==" | q"equals" => (path, args) => q"$filter.eq($path, ${args.get})"
    case q"<"              => (path, args) => q"$filter.lt($path, ${args.get})"
    case q"<="             => (path, args) => q"$filter.le($path, ${args.get})"
    case q">"              => (path, args) => q"$filter.gt($path, ${args.get})"
    case q">="             => (path, args) => q"$filter.ge($path, ${args.get})"
    case q"isEmpty"        => (path, args) => q"$filter.isNull($path)"
    case q"isDefined" | q"nonEmpty" => (path, args) =>
        q"$filter.gt($path, _root_.com.google.cloud.datastore.NullValue.of())"
    case q"contains" | q"foreach" => (path, args) => q"$filter.eq($path, ${args.get})"
  }

  protected case class CallTree(tree: mutatus.utils.BinaryTree[c.Tree]) {
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

  protected object CallTree {
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

  protected case class AppliedCriteria(
      path: String,
      operation: Option[c.Tree],
      arg: Option[c.Tree]
  )

  protected def compoundTypeParents(tpe: c.Tree): List[c.Tree] = tpe match {
    case tq"..$parents {}" => parents.flatMap(compoundTypeParents)
    case parent            => parent :: Nil
  }

  protected def propertyName(property: c.Tree) = property match {
    case tq"_root_.mutatus.Property[$params, $_]" =>
      compoundTypeParents(params).head
    case tpe => tpe
  }
}
