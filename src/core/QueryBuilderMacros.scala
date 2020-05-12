package mutatus

import scala.reflect.macros._
import mutatus.utils.MacroHelpers

class QueryBuilderMacros(val c: whitebox.Context) extends MacroHelpers {
  import c.universe._
  private val self = c.prefix.asInstanceOf[c.Expr[QueryBuilder[_]]]
  
  private val idxDef = self.tree.collect{
      case q"type FullIdxDef = $tpt" => tpt
    }.lastOption.map{
      case tpe @ TypeTree() if tpe.original != null => tpe.original
      case tpe => tpe  
    }.map{
      case tpe: CompoundTypeTree => tpe
    }.getOrElse(tq"_root_.mutatus.Schema.IndexType.Simple")
  
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
            case property @ tq"_root_.mutatus.Schema.Property[$path, $order]" => 
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
                  tq"$Property[$updatedParam, $order]" -> true
                } else tq"$Property[$path with $equalityOperator, $order]" -> true 
              else property -> false

            case other => other -> false
          }.unzip

          val allParts = if(appliesToCritieria.exists(_ == true)) checkedParts
            else checkedParts :+ tq"$Property[$pathLiteral with $equalityOperator, _root_.mutatus.Schema.OrderDirection]"
          
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
          case property @ tq"_root_.mutatus.Schema.Property[$path, $order]" => 
            val params = compoundTypeParents(path)
            val matchesCriteriaPath = params.exists(_.equalsStructure(pathLiteral))
            val hasDefinedSortOrder = order.equalsStructure(AscendingOrder) || order.equalsStructure(DescendingOrder)

            if(matchesCriteriaPath && !hasDefinedSortOrder)
              tq"$Property[$path with $MSSortOrder, $AscendingOrder]" -> true
            else if(criteria.last == ac) {
              val filteredPath = compoundTypeParents(path)
                .filterNot(_.equalsStructure(MSSortOrder))
                .reduce[c.Tree]{case (lhs, rhs) => tq"$lhs with $rhs"}
              tq"$Property[$filteredPath, $order]" -> false
            } else property -> false

          case other => other -> false
        }.unzip

        val allParts = if(appliesToCritieria.exists(_ == true)) checkedParts
        else checkedParts :+ tq"_root_.mutatus.Schema.Property[$pathLiteral with $MSSortOrder, $AscendingOrder]"
        
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
      case tq"_root_.mutatus.Schema.Property[$path, $order]" =>
        val newPropertyOrder = 
          if(order.equalsStructure(AscendingOrder)) DescendingOrder
          else if(order.equalsStructure(DescendingOrder)) AscendingOrder
          else order
          tq"$Property[$path, $newPropertyOrder]"

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

  def takeImpl[T: c.WeakTypeTag](limit: c.Tree): c.universe.Tree = {
    q"""
    val current = $self
    new QueryBuilder[${weakTypeOf[T]}](current.kind, current.query.copy(limit = _root_.scala.Some($limit))) {
          type IdxDef = ${simplifyIndexDef(idxDef)}
          type FullIdxDef = ${idxDef}
    }"""
  }

  def dropImpl[T: c.WeakTypeTag](offset: c.Tree): c.universe.Tree = {
    q"""
    val current = $self
    new QueryBuilder[${weakTypeOf[T]}](current.kind, current.query.copy(offset = _root_.scala.Some($offset))) {
          type IdxDef = ${simplifyIndexDef(idxDef)}
          type FullIdxDef = ${idxDef}
    }"""
  }

  def sliceImpl[T: c.WeakTypeTag](offset: c.Tree, limit: c.Tree): c.universe.Tree = {
    q"""
    val current = $self
    new QueryBuilder[${weakTypeOf[T]}](current.kind, current.query.copy(offset = _root_.scala.Some($offset), limit = _root_.scala.Some($limit))) {
          type IdxDef = ${simplifyIndexDef(idxDef)}
          type FullIdxDef = ${idxDef}
    }"""
  }
  
  /**
   * Macro used run query with validation that schema contains index definition which contains properties in correct order without redundancy. 
   * Such validations are needed to unsure that query may be executed by Datastore
   * 
   * This method is suboptimal, envolves large amount of string checking. I've not found good method to check Schema type (based on RefinedType) with IndexDef TypeTrees 
   */
  def runImpl[T: c.WeakTypeTag]()(
      svc: c.Tree,
      namespace: c.Tree,
      decoder: c.Tree,
      ev: c.Tree
  ): c.Tree = {
    val entityType = c.weakTypeOf[T]
    val indexPrefix = s"mutatus.Schema.Index[$entityType"

    val properties = compoundTypeParents(simplifyIndexDef(idxDef))
      .collect {
        case tq"_root_.mutatus.Schema.Property[$path, $order]" => path -> order.tpe
      }

    val expectedProperties = properties.collect {
      case (path, order) => s"""mutatus.Schema.Property[String($path),$order]"""
    }

    val existsValidIndex = ev.tpe.exists { tpe =>
      val str = show(tpe)
      def appliesTo = str.startsWith(indexPrefix)

      lazy val idxDef = str.drop(indexPrefix.length())
      lazy val idxProperites = idxDef.split("with").tail
      
      def hasEqualNumberOfProperties = expectedProperties.size == idxProperites.size
      def containsSingleDef = !idxDef.contains("mutatus.Schema.Index[")
      def matchesAllProperties =
        expectedProperties
          .zip(idxProperites)
          .forall {
            case (idxProperty, schemaProperty) => schemaProperty.contains(idxProperty)
          }

          appliesTo && containsSingleDef && hasEqualNumberOfProperties && matchesAllProperties
    }

    if (existsValidIndex) {
      q"""{
        val current = $self
        current.runUnsafe()
      }"""
    } else{
      val expectedIdx = properties.map{
        case (path, direction) if direction.toString.contains("Ascedning") => s"$path ASC"
        case (path, _) => s"$path DESC"
      }.mkString(", ")

      c.abort(
        c.enclosingPosition,
         s"""mutatus: Schema does not contain valid index. Order or number of defined index properties may not satisfy Datastore query.   
             | - kind: $entityType,
             | - properties: $expectedIdx""".stripMargin)
    } 
  }

  /** Builds IndexDefinition based on included parts. Validates query and resolves does it need Complex or Simple Index
   * Complex Query cases:
   * - Queries with ancestor and inequality filters (TODO)
   * - Queries with one or more inequality filters on a property and one or more equality filters on other properties
   * - Queries with a sort order on keys in descending order (TODO checking if property is Key, It works with other properties)
   * - Queries with multiple sort orders
   * - Queries with one or more filters and one or more sort orders
   * */ 
  private def buildNewIndexDef(parts: List[c.Tree]): c.Tree = {
    def propertiesWithCriteria(criteria: c.Tree) = parts.filter(_.exists(_.equalsStructure(criteria)))

    val inequalityFilters = propertiesWithCriteria(InequalityFiltered)
    val equalityFilters = propertiesWithCriteria(EqualityFiltered)
    val filterProperties = inequalityFilters ++ equalityFilters
    
    val asceningOrderProperties = propertiesWithCriteria(AscendingOrder)
    val descendingOrderProperties = propertiesWithCriteria(DescendingOrder)
    val orderProperties = asceningOrderProperties ++ descendingOrderProperties
    val nonFilteredOrderedProperties = orderProperties.diff(filterProperties).reverse

    Validator.singleInequalityProperty(inequalityFilters)
    Validator.validInequalityFilterSortOrder(parts, orderProperties)

    val hasDefinedOrder = parts.exists{
      case tq"_root_.mutatus.Schema.Property[$_, $order]" => order.equalsStructure(AscendingOrder) || order.equalsStructure(DescendingOrder)
      case _ => false
    } 
    
    def usesSingleProperty = orderProperties.size == 1 && filterProperties.size == 1 && orderProperties.diff(filterProperties).isEmpty
    def hasCombinationOfFilterTypes = inequalityFilters.nonEmpty && equalityFilters.nonEmpty
    def hasMultipleSortOrders = orderProperties.size > 1
    def hasCombinationOrFiltersAndSortOrders = filterProperties.nonEmpty && orderProperties.nonEmpty && !usesSingleProperty

    val needsComplexIndex = hasCombinationOfFilterTypes ||
      hasMultipleSortOrders ||
      hasCombinationOrFiltersAndSortOrders

    val idxDefType = if(needsComplexIndex) tq"_root_.mutatus.Schema.IndexType.Complex"
      else tq"_root_.mutatus.Schema.IndexType.Simple"

    (equalityFilters ::: inequalityFilters ::: nonFilteredOrderedProperties)
      .foldLeft[c.Tree](idxDefType){
        case (lhs, rhs) => tq"$lhs with $rhs"
    }
  }

  /** Simplifies given IndexDef. If it's complex index then it removes Filtered property infix types
   * In case of SimpleIndexes it removes all properties, as they would does not matter in validation of index existance.
   */ 
  private def simplifyIndexDef(tpe: c.Tree): c.Tree = {
    val SimpleIndex = tq"_root_.mutatus.Schema.IndexType.Simple"
    if(tpe.exists(_.equalsStructure(SimpleIndex))) SimpleIndex
    else {
      compoundTypeParents(tpe).map{
        case tq"_root_.mutatus.Schema.Property[$params, $order]" => 
          val pathLiteral = compoundTypeParents(params).head
          tq"$Property[$pathLiteral, $order]"
        case other => other
      }.reduce[c.Tree]{case (lhs, rhs) => tq"$lhs with $rhs"}
    }
  }



  object Validator{
    def singleInequalityProperty(inequalityFilters: List[c.Tree]): Unit = {
      if(inequalityFilters.size > 1){
        val fields = inequalityFilters.map(propertyName).mkString(", ") 
        c.abort(
          c.enclosingPosition,
          s"mutatus: Inequality criteria on multiple properties are prohibited, found on properties: $fields"
        )
      }
    }

    def validInequalityFilterSortOrder(idxDefParts: List[c.Tree], orderProperties: List[c.Tree]): Unit = {
      val position = c.macroApplication.pos
      lazy val nextSortOrder = position.source.lines(position.line)
        .takeWhile(!_.contains("run()"))
        .find(_.contains("sortBy("))
      
      if(orderProperties.nonEmpty){
        lazy val mssoProperty = idxDefParts.find(_.exists(_.equalsStructure(MSSortOrder))).get
        
        idxDefParts
        .find(_.exists(_.equalsStructure(InequalityFiltered)))
        .map{
          case property @ tq"_root_.mutatus.Schema.Property[$propertyParts, $_]" =>
            val isMSSOProperty = compoundTypeParents(propertyParts).exists(_.equalsStructure(MSSortOrder))

            if(!isMSSOProperty && nextSortOrder.isEmpty){
              c.abort(
                  c.enclosingPosition, 
                  s"mutatus: Most significent sort order property ${propertyName(mssoProperty)} does not match inequality filter property ${propertyName(property)}"
                )
            }
          }
      }
    }
  }
}
