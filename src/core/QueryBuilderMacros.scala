package mutatus

import scala.annotation.tailrec
import scala.reflect.macros._
import mutatus.utils.BinaryTree
import mutatus.utils.BinaryTree._
import java.io.FileWriter

class QueryBuilderMacros(val c: whitebox.Context) {
  import c.universe._
  private val self = c.prefix.asInstanceOf[c.Expr[QueryBuilder[_]]]

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
  private val filter = q"_root_.com.google.cloud.datastore.StructuredQuery.PropertyFilter"
  private val composite = q"_root_.com.google.cloud.datastore.StructuredQuery.CompositeFilter.and"
  private val orderBy = q"_root_.com.google.cloud.datastore.StructuredQuery.OrderBy"
  
  final val AscendingOrder = tq"_root_.mutatus.OrderDirection.Ascending.type"
  final val DescendingOrder = tq"_root_.mutatus.OrderDirection.Descending.type"
  final val InequalityFiltered = tq"_root_.mutatus.InequalityFiltered"
  final val EqualityFiltered = tq"_root_.mutatus.EqualityFiltered"

  private val equalityOperators: Set[c.Tree] = Set(q"==", q"equals", q"isEmpty", q"contains", q"foreach")
  private val operationMapping: PartialFunction[c.Tree, (String, Option[c.Tree]) => c.Tree] = {
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

  private val idxDef = self.tree.collect{
      case q"type FullIdxDef = $tpt" => tpt
    }.lastOption.map{
      case tpe @ TypeTree() if tpe.original != null => tpe.original
      case tpe => tpe  
    }.map{
      case tpe: CompoundTypeTree => tpe
    }.getOrElse(tq"_root_.mutatus.SimpleIndexDef")
  
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

    lazy val newIdxDef = criteria
      .foldLeft(idxDef) {
        case (tpe, ac) =>
          val pathLiteral = tq"${ac.path}"
          
          def usesEqualityOperator = ac.operation.forall(op => equalityOperators.exists(_.equalsStructure(op))) 
          val equalityOperator = if(usesEqualityOperator) EqualityFiltered else InequalityFiltered

          val (checkedParts, appliesToCritieria) = compoundTypeParents(tpe).collect{
            case property @ tq"_root_.mutatus.Property[$path, $order]" => 
              val params = compoundTypeParents(path)
              
              val matchesCriteriaPath = params.exists(_.equalsStructure(tq"$pathLiteral"))
              val containsEqualityOperator = params.exists{param => 
                List(InequalityFiltered, EqualityFiltered).exists(_.equalsStructure(param))
              }

              if(matchesCriteriaPath)
                if(containsEqualityOperator){
                  val updatedParam = params.collect{
                    case tpe if tpe.equalsStructure(InequalityFiltered) => InequalityFiltered
                    case tpe if tpe.equalsStructure(EqualityFiltered) => equalityOperator
                    case other => other
                  }.reduce[c.Tree]{case (lhs, rhs) => tq"$lhs with $rhs"}
                  tq"_root_.mutatus.Property[$updatedParam, $order]" -> true
                } else tq"_root_.mutatus.Property[$path with $equalityOperator, $order]" -> true 
              else property -> false

            case other => other -> false
          }.unzip

          val allParts = if(appliesToCritieria.exists(_ == true)) checkedParts
            else checkedParts :+ tq"_root_.mutatus.Property[$pathLiteral with $equalityOperator, _root_.mutatus.OrderDirection]"
          
          buildNewIndexDef(allParts)
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
            type IdxDef = ${simplifyIndexDef(newIdxDef)}
            type FullIdxDef = ${newIdxDef}
          }
        }"""
      }
      .getOrElse(q"$self")
  }

   def sortByImpl[T: c.WeakTypeTag](pred: c.Tree*): c.universe.Tree = {
    val Sortable = tq"_root_.mutatus.Sortable"

    val criteria = pred.flatMap(CallTree(_).resolveCriteria)
    val sortBy: Seq[c.Tree] = criteria
      .map {
        case AppliedCriteria(path, _, _) => q"$orderBy.asc($path)"
      }

    lazy val newIdxDef = criteria
      .foldLeft(idxDef) {
        case (tpe, ac) =>
        val pathLiteral = tq"${ac.path}"
        val (checkedParts, appliesToCritieria) = compoundTypeParents(tpe).collect{
          case property @ tq"_root_.mutatus.Property[$path, $order]" => 
            val params = compoundTypeParents(path)
            val matchesCriteriaPath = params.exists(_.equalsStructure(pathLiteral))
            val hasSortableCrtieria = params.exists(_.equalsStructure(Sortable))

            if(matchesCriteriaPath && !hasSortableCrtieria)
              tq"_root_.mutatus.Property[$path with $Sortable, $AscendingOrder]" -> true
            else property -> false

          case other => other -> false
        }.unzip

        val allParts = if(appliesToCritieria.exists(_ == true)) checkedParts
        else checkedParts :+ tq"_root_.mutatus.Property[$pathLiteral with _root_.mutatus.Sortable, _root_.mutatus.OrderDirection.Ascending.type]"
        
        buildNewIndexDef(allParts)
      }

    q"""{
      val current = $self
      val newOrder = List(..$sortBy).foldLeft(current.orderCriteria){
        case (acc, orderBy) => orderBy :: acc.filterNot(_.getProperty() == orderBy.getProperty()) 
      }
      new QueryBuilder[${weakTypeOf[T]}](current.kind, current.query.copy(orderCriteria = newOrder)){
        type IdxDef = ${simplifyIndexDef(newIdxDef)}
        type FullIdxDef = ${newIdxDef}      }
    }"""
  }

  def reverseImpl[T: c.WeakTypeTag]: c.universe.Tree = {
    val newIdxDef = compoundTypeParents(idxDef).map{
      case tq"_root_.mutatus.Property[$path, $order]" =>
        val newPropertyOrder = 
          if(order.equalsStructure(AscendingOrder)) DescendingOrder
          else if(order.equalsStructure(DescendingOrder)) AscendingOrder
          else order
          tq"_root_.mutatus.Property[$path, $newPropertyOrder]"

          case other => other
    }
    .reduceLeft[c.Tree]{
      case (tpe, part) => tq"$tpe with $part"
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
          type IdxDef = ${simplifyIndexDef(newIdxDef)}
          type FullIdxDef = ${newIdxDef}
        }
    }"""
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

  private def compoundTypeParents(tpe: c.Tree): List[c.Tree] = tpe match {
    case tq"..$parents {}" => parents.flatMap(compoundTypeParents)
    case parent => parent :: Nil
  }

  /** Builds IndexDefinition based on included parts. Validates query and resolves does it need Complex or Simple Index
   * Complex Query cases:
   * - Queries with ancestor and inequality filters (TODO)
   * - Queries with one or more inequality filters on a property and one or more equality filters on other properties
   * - Queries with a sort order on keys in descending order
   * - Queries with multiple sort orders
   * - Queries with one or more filters and one or more sort orders
   * */ 
  private def buildNewIndexDef(parts: List[c.Tree]): c.Tree = {
    def propertiesWithCriteria(criteria: c.Tree) = parts.filter(_.exists(_.equalsStructure(criteria)))
    def propertyName(property: c.Tree) = property match {
      case tq"_root_.mutatus.Property[$params, $_]" => compoundTypeParents(params).head
      case tpe => tpe 
    } 

    val inequalityFilters = propertiesWithCriteria(InequalityFiltered)
    lazy val equalityFilters = propertiesWithCriteria(EqualityFiltered)
    lazy val filterProperties = inequalityFilters ++ equalityFilters

    lazy val asceningOrderProperties = propertiesWithCriteria(AscendingOrder)
    lazy val descendingOrderProperies = propertiesWithCriteria(DescendingOrder)
    lazy val orderProperies = asceningOrderProperties ++ descendingOrderProperies

    if(inequalityFilters.size > 1){
      val fields = inequalityFilters.map(propertyName).mkString(", ") 
      c.abort(
        c.enclosingPosition,
        s"mutatus: Inequality criteria on multiple properties are prohibited, found on: $fields"
      )
    } 
    //TODO: Check if 1st sort order criteria matchers inequality property. Abort if does not.

    def hasCombinationOfFilterTypes = inequalityFilters.nonEmpty && equalityFilters.nonEmpty
    def usesDescedningSortOrder = descendingOrderProperies.nonEmpty
    def hasMultipleSortOrders = orderProperies.size > 1
    def hasCombinationOrFiltersAndSortOrders = filterProperties.nonEmpty && orderProperies.nonEmpty

    val needsComplexIndex = hasCombinationOrFiltersAndSortOrders ||
      usesDescedningSortOrder || 
      hasMultipleSortOrders ||
      hasCombinationOfFilterTypes

    val idxDefType = if(needsComplexIndex) tq"_root_.mutatus.ComplexIndexDef"
      else tq"_root_.mutatus.SimpleIndexDef"

    parts.tail.foldLeft[c.Tree](idxDefType){
        case (lhs, rhs) => tq"$lhs with $rhs"
    }
  }

  /** Simplifies given IndexDef. If it's complex index then it removes Sortable, Filtered property infix types
   * In case of SimpleIndexes it removes all properties, as they would does not matter in validation of index existance.
   */ 
  private def simplifyIndexDef(tpe: c.Tree): c.Tree = {
    val SimpleIndex = tq"_root_.mutatus.SimpleIndexDef"
    if(tpe.exists(_.equalsStructure(SimpleIndex))) SimpleIndex
    else {
      compoundTypeParents(tpe).map{
        case tq"_root_.mutatus.Property[$params, $order]" => 
          val pathLiteral = compoundTypeParents(params).head
          tq"_root_.mutatus.Property[$pathLiteral, $order]"
        case other => other
      }.reduce[c.Tree]{case (lhs, rhs) => tq"$lhs with $rhs"}
    }
  }
}
