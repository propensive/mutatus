package mutatus

import antiphony._
import euphemism.Json
import euphemism.Json.{Serializer, Deserializer}
import language.experimental.macros
import mutatus.SchemaDef.IndexState
import scala.concurrent.duration._
import scala.annotation.tailrec
import org.typelevel.jawn.ast.JString
import Mutatus._

/** Contains type defininitions of all Entity indexes */
sealed trait Schema[+T <: Schema.Index[_, _]]
object Schema {
  sealed trait Contains[+U <: Schema.Index[_,_]]
  implicit def simpleIndexSchema[T]: Schema[Index[T, IndexType.Simple]] = new Schema[Index[T, IndexType.Simple]] {}

  /** Base type for each Index Property definition.
    * IndexDef is used within QueryBuilder to check is query can be executed with predefined Datastore Indexes
    * Each IndexDef may be extended with Property
    */
  sealed trait IndexType
  object IndexType {

    /** Represents index which needs defined Complex/Manual Index inside Datastore to be corectlly evaluated */
    trait Complex extends IndexType

    /** Represents index which does not need manually created Index within Datastore. Such query is executed using built-in Indexes*/
    trait Simple extends IndexType
  }

  trait Index[Entity, +Properies <: IndexType]

  /** Property defines single Index entry for given path P, defined as literal singleton and OrderDirection in which it is indexed inside Datastore  */
  trait Property[P <: Singleton, +Order <: OrderDirection] { _: IndexDef => }

  /** Internal trait used to mark property set to be me the most significent position. Used for internal checks and query validations */
  trait MostSignificentSortOrder

  /** Internal trait used to mark that given query property would be used within filter crtieria*/
  trait Filtered
  trait InequalityFiltered extends Filtered
  trait EqualityFiltered extends Filtered

  /** Order direction for given Index Proprty existing in schema */
  sealed trait OrderDirection {
    val StringValue = this.toString().toUpperCase()
  }

  object OrderDirection {
    implicit val encoder: Serializer[OrderDirection] = v => JString(v.StringValue)
    implicit val decoder: Deserializer[OrderDirection] = v => values.find(_.StringValue == v.asString.toUpperCase)
    case object Ascending extends OrderDirection
    case object Descending extends OrderDirection

    lazy val values = List(Ascending, Descending)
  }
}

case class SchemaDef[+T <: Schema.Index[_, _]](indexes: SchemaDef.IndexDef[_]*) {
  type IdxList = List[SchemaDef.IndexDef[_]]

  /** Provides proof in form of `implicit Schema` that Datastore contains all indices defined in SchemaDef which means all queries defineded in fn block will be satisfied.  
   *  This is method does not create missing indices and does not wait until all indices are ready,
   *  in order to achive such behaviour use curried version of this method `SchemaDef.using(createMissing, waitReady, timeout)(fn)`
  */
  def using[R](fn: Schema[T] => Result[R])(implicit service: Service): Result[R] = using(createMissing = false, waitReady = false)(fn)
  
  /**
    * Provides proof in form of `implicit Schema` that Datastore contains all indices defined in SchemaDef which means all queries defineded in fn block will be satisfied.  
    * @param createMissing If some indexes are missing in Datastore create them using REST API
    * @param waitReady Wait until all indices state is ready
    * @param timeout Maximal time to wait for indices to change it's state to Ready
    */
  def using[R](createMissing: Boolean = true, waitReady: Boolean = true, timeout: FiniteDuration = 5.minute)(fn: Schema[T] => Result[R])(implicit service: Service):Result[R] = for {
      existingIndexes <- fetchIndices()
      missing = indexes.filterNot(_.matchesOneOf(existingIndexes)).toList
      notReady = existingIndexes
        .filterNot(_.state == IndexState.Ready)
        .filter(_.matchesOneOf(indexes))
      created <- createIndexesIfNeeded(missing, createMissing)
      _ <- waitIndexesReadyIfNeeded(waitReady, timeout, notReady.toList ++ created)
      result <- usingUnsafe(fn)
    } yield result

  private[mutatus] def usingUnsafe[R](fn: Schema[T] => Result[R])(implicit service: Service):Result[R] = fn(new Schema[T] {})

  private def createIndexesIfNeeded(indexes: IdxList, createMissing: Boolean = true)(implicit service: Service): Result[IdxList] = {
    def iter(indexes: IdxList, finished: IdxList): Result[IdxList] = {
      indexes match {
        case Nil => Answer(finished)
        case index :: tail => index.create() match {
          case Answer(value) => iter(tail, value :: finished)
          case err: Erroneous => err.map(_ => Nil)
        }
      }
    }
    if(createMissing && indexes.nonEmpty)  iter(indexes, Nil)
    else Result(Nil)
  }

  private def fetchIndices()(implicit service: Service): Mutatus.Result[Set[SchemaDef.IndexDef[_]]] =
    for {
      response <- Http.get(
          url = s"https://datastore.googleapis.com/v1/projects/${service.options.getProjectId()}/indexes",
          headers = service.authorization
        ).adapt[Mutatus.type]
      json <- Json.parse(new String(response)).adapt[Mutatus.type]
      indexes <- json.indexes.as[List[SchemaDef.IndexDef[Any]]].adapt[Mutatus.type]
    } yield indexes.toSet

  private def waitIndexesReadyIfNeeded(waitReady:Boolean, timeout: FiniteDuration, checkedIndexes: IdxList): Mutatus.Result[Unit] = if(!waitReady || checkedIndexes.isEmpty){
      Answer()
    }else{
      val start = System.currentTimeMillis()
      println(s"Awaiting for creation of ${checkedIndexes.size} Datastore indexes, max timeout $timeout")

      @tailrec
      def await(): Result[Unit] ={
        val result = for{
          indexes <- fetchIndices()
          matching = indexes.filter(_.matchesOneOf(checkedIndexes))
          allReady = matching.nonEmpty && matching.forall(_.state == IndexState.Ready)
          errorneousIndex = matching.find(_.state == IndexState.Error)
        } yield (allReady, errorneousIndex)
        
        result match {
          case Answer((_, Some(errIdx))) => Error(InvalidIndexDefinition(errIdx))
          case Answer((true, _)) => Answer()
          case Answer((false, _)) => 
              val tookTime = (System.currentTimeMillis - start).millis
              if(tookTime < timeout){
                Thread.sleep(1000)
                await()
              } else Error(IndexCreationTimeout)
          case err: Erroneous => err.map(_ => ())
        }
      }
      await()
    }
}

object SchemaDef {
  case class IndexDef[+T](kind: String, properties: List[Property], state: IndexState = IndexState.Ready, indexId: Option[String] = None) {
    def matchesOneOf(that: Traversable[IndexDef[_]]): Boolean = that.exists(_.matches(this))
    def matches(that: IndexDef[_]): Boolean = {
      def matchesById = indexId.exists(that.indexId.contains)
      def matchesByDefinition = kind == that.kind && properties == that.properties
      matchesById || matchesByDefinition
    }

    def create()(implicit service: Service): Result[IndexDef[T]] = {
      for {
        content <- Json
          .parse {
            s"""{
              |"kind": "${kind}",
              |"ancestor": "None",
              |"properties": ${Json(properties)}
              |}""".stripMargin
          }
          .adapt[Mutatus.type]
        headers = service.authorization
        response <- Http
          .post(s"https://datastore.googleapis.com/v1/projects/${service.options.getProjectId()}/indexes", content, headers)
          .adapt[Mutatus.type]
        json <- Json.parse(new String(response)).adapt[Mutatus.type]
        id <- json.metadata.indexId.as[Option[String]].adapt[Mutatus.type]
      } yield copy(state = IndexState.Creating, indexId = id)
    }

    def delete()(implicit service: Service): Result[Unit] = {
      if (indexId.isEmpty) {
        Error(SerializationException("Cannot delete index without indexId"))
      } else {
        for {
          response <- Http
            .request(
              url = s"https://datastore.googleapis.com/v1/projects/${service.options.getProjectId()}/indexes/${indexId.get}",
              content = "{}",
              method = "DELETE",
              headers = service.authorization
            )
            .adapt[Mutatus.type]
          result = println(new String(response))
        } yield result
      }
    }
  }
  case class Property(name: String, direction: Schema.OrderDirection)
  def apply[T](indexes: Index[T]*): SchemaDef[_] = macro IndexesMacros.extractSchemaImpl

  sealed trait IndexState {
    val StringValue = this.toString().toUpperCase()
  }
  object IndexState {
    case object Creating extends IndexState
    case object AlreadyExists extends IndexState {
      override val StringValue: String = "ALREADY_EXISTS"
    }
    case object Error extends IndexState
    case object Ready extends IndexState

    lazy val values = List(Creating, AlreadyExists, Error, Ready)

    implicit val encoder: Serializer[IndexState] = v => JString(v.StringValue)
    implicit val decoder: Deserializer[IndexState] = v => values.find(_.StringValue == v.asString.toUpperCase())
  }
}

case class Index[-T](properties: PropertySelector[T]*)
sealed abstract class PropertySelector[-T](val orderDirection: Schema.OrderDirection) {
  def selector: T => Any
}
case class Asc[-T](selector: T => Any) extends PropertySelector[T](Schema.OrderDirection.Ascending)
case class Desc[-T](selector: T => Any) extends PropertySelector[T](Schema.OrderDirection.Descending)
