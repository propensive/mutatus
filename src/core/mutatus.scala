/* Mutatus, version 0.10.0. Copyright 2018 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://propensive.com/
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
package mutatus

import adversaria._
import com.google.cloud.datastore._
import magnolia._
import quarantine._
import mercator._

import scala.annotation.StaticAnnotation
import scala.collection.JavaConverters._
import scala.reflect.runtime.universe.WeakTypeTag
import scala.collection.generic.CanBuildFrom
import scala.language.experimental.macros
import io.opencensus.trace.Status.CanonicalCode
import com.google.rpc.Code
import com.google.auth.oauth2.{ServiceAccountCredentials, AccessToken}

/** Mutatus package object */
object `package` extends Domain[MutatusException] {
  implicit val mitigateHttp = antiphony.Http.mitigate(mutatus.`package`) {
      case _: antiphony.NotAuthorized => Unathorized
      case ex => DatabaseException(ex)
  }
  
  implicit val mitigateEuphenismParser = euphemism.ParseException.mitigate(mutatus.`package`){
    case ex:  euphemism.ParseException => SerializationException(ex.message)
  }
  implicit val mitigateEuphenismAccess = euphemism.Access.mitigate(mutatus.`package`){
    case ex =>     SerializationException(ex.getMessage())
  }

  /** provides `saveAll` and `deleteAll` methods for collections of case class instances */
  implicit class DataBatchExt[T <: Product](values: Traversable[T]) {

    /** saves the all case class as a Datastore entity in batch mode */
    def saveAll()(
        implicit svc: Service,
        encoder: Encoder[T],
        dao: Dao[T],
        idField: IdField[T]
    ): mutatus.Result[Map[T, Ref[T]]] = {
      val (batch, refs) =
        values.foldLeft(svc.readWrite.newBatch() -> Map.empty[T, Ref[T]]) {
          case ((b, entityRefs), value) =>
            val entity = value.buildEntity()
            b.put(value.buildEntity())
            b -> entityRefs.updated(value, new Ref[T](entity.getKey))
        }
      Result(batch.submit())
        .extenuate {
          case ex: DatastoreException => DatabaseException(ex)
        }
        .map(_ => refs)
      //batch may supply only those keys which were generated by Datastore, but in mutatus generates keys deterministically based on entity content
    }

    /** deletes the Datastore entities in batch mode*/
    def deleteAll()(
        implicit svc: Service,
        dao: Dao[T],
        idField: IdField[T]
    ): mutatus.Result[Unit] =
      Result {
        values
          .foldLeft(svc.readWrite.newBatch()) {
            case (b, value) =>
              b.delete(idField.idKey(idField.key(value)).newKey(dao.keyFactory))
              b
          }
          .submit()
        ()
      }.extenuate {
        case ex: DatastoreException =>
          import google.rpc._
          ex.getCode match {
            case Code.NOT_FOUND_VALUE => NotSavedException(dao.kind)
            case _                    => DatabaseException(ex)
          }
      }
  }

  /** provides `save` and `delete` methods on case class instances */
  implicit class DataExt[T <: Product](value: T) {
    private[mutatus] def buildEntity()(
        implicit encoder: Encoder[T],
        dao: Dao[T],
        idField: IdField[T]
    ): Entity =
      encoder.encode(value) match {
        case entityValue: EntityValue =>
          Entity
            .newBuilder(
              idField.idKey(idField.key(value)).newKey(dao.keyFactory),
              entityValue.get()
            )
            .build()
      }

    /** saves the case class as a Datastore entity */
    def save()(
        implicit svc: Service,
        encoder: Encoder[T],
        dao: Dao[T],
        idField: IdField[T]
    ): mutatus.Result[Ref[T]] =
      Result {
        new Ref[T](
          svc.readWrite.put(buildEntity()).getKey
        )
      }.extenuate {
        case exc: DatastoreException => DatabaseException(exc)
      }

    /** deletes the Datastore entity with this ID */
    def delete()(
        implicit svc: Service,
        dao: Dao[T],
        idField: IdField[T]
    ): mutatus.Result[Unit] =
      Result {
        svc.readWrite.delete(
          idField.idKey(idField.key(value)).newKey(dao.keyFactory)
        )
      }.extenuate {
        case ex: DatastoreException =>
          import google.rpc._
          ex.getCode match {
            case Code.NOT_FOUND_VALUE => NotSavedException(dao.kind)
            case _                    => DatabaseException(ex)
          }
      }
  }

  private[mutatus] def ifEmpty[T](
      str: String,
      empty: T,
      nonEmpty: String => T
  ): T =
    if (str.isEmpty) empty else nonEmpty(str)

  implicit val monadicResult: Monadic[mutatus.Result] =
    new Monadic[mutatus.Result] {
      def flatMap[A, B](from: Result[A])(fn: A => Result[B]): Result[B] =
        from.flatMap(fn)
      def map[A, B](from: Result[A])(fn: A => B): Result[B] = from.map(fn)
      def point[A](value: A): Result[A] = Result(value)
    }
}

final class id() extends StaticAnnotation

/** a reference to another case class instance stored in the GCP Datastore */
case class Ref[T](ref: Key) {

  /** resolves the reference and returns a case class instance */
  def apply()(implicit svc: Service, decoder: Decoder[T]): Result[T] =
    decoder.decode(svc.read.get(ref))
  override def toString: String = s"$Ref[${ref.getKind}]($key)"

  /** a `String` version of the key contained by this reference */
  def key: String = ref.getNameOrId.toString
}

/** companion object for `Namespace`, providing a default namespace */
object Namespace { implicit val defaultNamespace: Namespace = Namespace("") }

/** a GCP namespace */
case class Namespace(name: String) {
  def option: Option[String] = ifEmpty(name, None, Some(_))
}

/** companion object for Geo instances */
object Geo {
  def apply(latLng: LatLng): Geo = Geo(latLng.getLatitude, latLng.getLongitude)
}

/** a geographical position, with latitude and longitude */
case class Geo(lat: Double, lng: Double) {
  def toLatLng: LatLng = LatLng.of(lat, lng)
}

/** a representation of the GCP Datastore service */
case class Service(readWrite: Datastore) {
  val read: DatastoreReader = readWrite
  val options = readWrite.getOptions()
  def accessToken: AccessToken = {
    val credentials = options.getCredentials() match {
      case credentials: ServiceAccountCredentials => credentials.createScoped("https://www.googleapis.com/auth/datastore")
    }
    Option(credentials.getAccessToken()).getOrElse(credentials.refreshAccessToken())
  }
}

object Service {

  /** provides a default instance of the GCP Datastore service */
  implicit val default: Service = Service(
    DatastoreOptions.getDefaultInstance.getService
  )
}

/** typeclass for encoding a value into a type which can be stored in the GCP Datastore */
trait Encoder[T] {
  def encode(value: T): Value[_]
  def contraMap[T2](fn: T2 => T): Encoder[T2] = v => encode(fn(v))
}

/** typeclass for decoding a value from the GCP Datastore into a Scala type */
trait Decoder[T] {
  def decode(entity: Entity): Result[T] = decodeValue(EntityValue.of(entity))
  def decodeValue(value: Value[_]): Result[T]
  def map[T2](fn: T => T2): Decoder[T2] = {
    case null => Error(SerializationException(s"Cannot map over null value"))
    case v    => decodeValue(v).map(fn)
  }
}

/** typeclass for generating an ID field from a case class */
abstract class IdField[-T] {
  type Return
  def key(t: T): Return
  def toId: ToId[Return]
  def idKey(r: Return): IdKey = toId.toId(r)
}

object IdField {
  type FindMetadataAux[T, R] = FindMetadata[id, T] { type Return = R }

  implicit def annotationId[T, R](implicit ann: FindMetadata[id, T] {
    type Return = R
  }, implicitToId: ToId[R]): IdField[T] { type Return = ann.Return } =
    new IdField[T] {
      type Return = ann.Return
      def toId: ToId[Return] = implicitToId
      def key(t: T): Return = ann.get(t)
    }

  def from[T, R](
      fn: T => R
  )(implicit implicitToId: ToId[R]): IdField[T] { type Return = R } =
    new IdField[T] {
      type Return = R
      def toId: ToId[Return] = implicitToId
      def key(t: T): R = fn(t)
    }
}

object ToId {
  implicit val long: ToId[Long] = LongId(_)
  implicit val int: ToId[Int] = v => LongId(v.toLong)
  implicit val short: ToId[Short] = v => LongId(v.toLong)
  implicit val byte: ToId[Byte] = v => LongId(v.toLong)
  implicit val string: ToId[String] = StringId(_)
  implicit val guid: ToId[Guid] = guid => StringId(guid.guid)
  implicit val auto: ToId[Auto] = v => LongId(v.id)
}

trait ToId[R] { def toId(value: R): IdKey }

case class Auto(id: Long = -1) extends AnyVal {
  override def toString: String = id.toString
}

sealed trait IdKey { def newKey(keyFactory: KeyFactory): Key }

case class StringId(id: String) extends IdKey {
  def newKey(kf: KeyFactory): Key = kf.newKey(id)
}

object Guid {
  def apply(str: String): Guid =
    new Guid(if (str == "") java.util.UUID.randomUUID().toString else str)

  def apply(): Guid = apply("")

  private val pattern = java.util.regex.Pattern
    .compile("[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}")

  def unapply(str: String): Option[Guid] =
    if (pattern.matcher(str).matches()) Some(Guid(str))
    else None
}

class Guid(val guid: String) extends IdKey {
  def newKey(kf: KeyFactory): Key = kf.newKey(guid)
  override def hashCode: Int = guid.hashCode
  override def equals(that: Any): Boolean = that match {
    case that: Guid => that.guid == guid
    case _          => false
  }
  override def toString: String = guid
}

case class LongId(id: Long) extends IdKey {
  def newKey(kf: KeyFactory): Key = kf.newKey(id)
}

/** a data access object for a particular type */
case class Dao[T: WeakTypeTag](kind: String)(
    implicit svc: Service,
    namespace: Namespace,
    decoder: Decoder[T]
) {

  private[mutatus] lazy val keyFactory = {
    val baseFactory = svc.readWrite.newKeyFactory().setKind(kind)
    namespace.option.foldLeft(baseFactory)(_.setNamespace(_))
  }

  /** returns query builder with empty criteria which fetches all the values of this type stored in the GCP Platform */
  def all = new QueryBuilder[T](kind, Query()) {
    type IdxDef = mutatus.Schema.IndexType.Simple
    type FullIdxDef = IdxDef
  }

  def unapply[R](id: R)(implicit idField: IdField[T] {
    type Return = R
  }): Option[Result[T]] = {
    val key = idField.idKey(id).newKey(keyFactory)
    Option(svc.read.get(key))
      .map(decoder.decode)
  }
}

/** companion object for data access objects */
object Dao {
  implicit def apply[T](
      implicit metadata: TypeMetadata[T],
      decoder: Decoder[T],
      namespace: Namespace,
      service: Service
  ): Dao[T] = Dao(metadata.typeName)
}

/** companion object for `Decoder`, including Magnolia generic derivation */
object Decoder extends Decoder_1 {
  type Typeclass[T] = Decoder[T]

  /** combines `Decoder`s for each parameter of the case class `T` into a `Decoder` for `T` */
  def combine[T](caseClass: CaseClass[Decoder, T]): Decoder[T] = {
    case entityValue: EntityValue =>
      val entity = entityValue.get()

      caseClass.constructMonadic { param =>
        if (entity.contains(param.label)) {
          param.typeclass.decodeValue(entity.getValue(param.label))
        } else {
          param.default match {
            case Some(default) => Answer(default)
            case _ =>
              Error(
                SerializationException(
                  s"Entitiy misses param ${param.label} and not defined default value defined"
                )
              )
          }
        }
      }
  }

  /** tries to decode entity based on encoded `_meta.typename` value containing name of class,
    * if such value is missing tries `Decoder`s for each subtype of sealed trait `T` until one doesn't throw an exception
   **/
  def dispatch[T](st: SealedTrait[Decoder, T]): Decoder[T] =
    encodedValue => {
      // Naive approach used when fetching entity not indexed by mutatus
      def firstSucess(value: Value[_]): mutatus.Result[T] = {
        st.subtypes.toStream
          .map { subtype => subtype.typeclass.decodeValue(value) }
          .collectFirst { case success: Answer[T] @unchecked => success }
          .getOrElse(
            Error(
              SerializationException(
                s"Not found any sealed trait subtype possible to decode from value: ${value}"
              )
            )
          )
      }

      def subtypeForName(
          typeName: String,
          entityValue: Value[_]
      ): mutatus.Result[T] = {
        st.subtypes
          .collectFirst {
            case subtype if subtype.typeName.short == typeName =>
              subtype.typeclass.decodeValue(entityValue)
          }
          .getOrElse(
            Error(
              SerializationException(
                s"Not found sealed trait subtype for name ${typeName}"
              )
            )
          )
      }

      encodedValue match {
        case entityValue: EntityValue if entityValue.get.contains(Encoder.metaField) =>
          Result(
            entityValue.get
              .getEntity(Encoder.metaField)
              .getString(Encoder.typenameField)
          ).flatMap(subtypeForName(_, entityValue))
        case encodedValue => firstSucess(encodedValue)
      }
    }

  private def simpleDecoder[In <: Value[_], Out](fn: In => Out): Decoder[Out] = {
    case value: In @unchecked => Result(fn(value))
  }

  implicit val int: Decoder[Int] = simpleDecoder[LongValue, Int](_.get().toInt)
  implicit val long: Decoder[Long] = simpleDecoder[LongValue, Long](_.get())
  implicit val byte: Decoder[Byte] =
    simpleDecoder[LongValue, Byte](_.get().toByte)
  implicit val short: Decoder[Short] =
    simpleDecoder[LongValue, Short](_.get().shortValue())
  implicit val string: Decoder[String] = {
    case value: StringValue => Answer(value.get())
    case value              => Answer(value.get().toString)
  }
  implicit val guid: Decoder[Guid] = string.map(Guid(_))
  implicit val char: Decoder[Char] =
    simpleDecoder[StringValue, Char](_.get().head)
  implicit val boolean: Decoder[Boolean] =
    simpleDecoder[BooleanValue, Boolean](_.get())
  implicit val double: Decoder[Double] =
    simpleDecoder[DoubleValue, Double](_.get())
  implicit val float: Decoder[Float] =
    simpleDecoder[DoubleValue, Float](_.get().toFloat)
  implicit val geo: Decoder[Geo] =
    simpleDecoder[LatLngValue, Geo](v => Geo(v.get()))
  implicit def ref[T](implicit idField: IdField[T]): Decoder[Ref[T]] = {
    case key: KeyValue => Answer(Ref[T](key.get()))
    case entity: EntityValue =>
      entity.get().getKey match {
        case completeKey: Key => Answer(Ref[T](completeKey))
        case _ =>
          Error(SerializationException("Cannot use incomplete key as Ref"))
      }
  }

  implicit def collection[Coll[T] <: Traversable[T], T: Decoder](
      implicit cbf: CanBuildFrom[Nothing, T, Coll[T]]
  ): Decoder[Coll[T]] = {
    case _: NullValue => Answer(List.empty[T].to[Coll])
    case list: ListValue =>
      val decoder = implicitly[Decoder[T]]
      def decodeCollectionFailFast(
          encoded: List[Value[_]],
          decoded: List[T] = Nil
      ): Result[Coll[T]] = encoded match {
        case Nil => Answer(decoded.reverse.to[Coll])
        case value :: tail =>
          decoder.decodeValue(value) match {
            case Answer(value) =>
              decodeCollectionFailFast(tail, value :: decoded)
            case err: Erroneous =>
              Error(
                SerializationException(
                  s"Failed to decode collection of ${value.getType()}: ${err.throwable.getMessage()}"
                )
              )
          }
      }

      decodeCollectionFailFast(list.get().asScala.toList)
  }

  implicit def optional[T: Decoder]: Decoder[Option[T]] = {
    case _: NullValue => Answer(None)
    case value        => implicitly[Decoder[T]].decodeValue(value).map(Option.apply)
  }
}

trait Decoder_1 {

  /** generates a new `Decoder` for the type `T` */
  implicit def gen[T]: Decoder[T] = macro Magnolia.gen[T]
}

/** companion object for `Encoder`, including Magnolia generic derivation */
object Encoder extends Encoder_1 {
  type Typeclass[T] = Encoder[T]

  final val metaField = "_meta"
  final val typenameField = "typename"

  /** combines `Encoder`s for each parameter of the case class `T` into a `Encoder` for `T` */
  def combine[T](caseClass: CaseClass[Encoder, T]): Encoder[T] =
    value =>
      EntityValue.of {
        caseClass.parameters
          .to[List]
          .foldLeft(FullEntity.newBuilder()) {
            case (b, param) =>
              b.set(
                param.label,
                param.typeclass.encode(param.dereference(value))
              )
          }
          .build()
      }

  /** chooses the appropriate `Encoder` of a subtype of the sealed trait `T` based on its type */
  def dispatch[T](sealedTrait: SealedTrait[Encoder, T]): Encoder[T] =
    value =>
      sealedTrait.dispatch(value) { st =>
        st.typeclass.encode(st.cast(value)) match {
          case encodedValue: EntityValue =>
            val meta = EntityValue
              .newBuilder {
                FullEntity
                  .newBuilder()
                  .set(typenameField, st.typeName.short)
                  .build()
              }
              .setExcludeFromIndexes(true)
              .build()

            EntityValue.of {
              FullEntity
                .newBuilder(encodedValue.get())
                .set(metaField, meta)
                .build()
            }
        }
      }

  implicit val string: Encoder[String] = StringValue.of
  implicit val guid: Encoder[Guid] = string.contraMap[Guid](_.guid)
  implicit val char: Encoder[Char] = v => StringValue.of(v.toString)
  implicit val long: Encoder[Long] = LongValue.of
  implicit val int: Encoder[Int] = v => LongValue.of(v.toLong)
  implicit val short: Encoder[Short] = v => LongValue.of(v.toLong)
  implicit val byte: Encoder[Byte] = v => LongValue.of(v.toLong)
  implicit val byteArray: Encoder[Array[Byte]] = v => BlobValue.of(Blob.copyFrom(v))
  implicit val boolean: Encoder[Boolean] = BooleanValue.of
  implicit val double: Encoder[Double] = DoubleValue.of
  implicit val float: Encoder[Float] = v => DoubleValue.of(v.toDouble)
  implicit val geo: Encoder[Geo] = v => LatLngValue.of(v.toLatLng)

  implicit def ref[T]: Encoder[Ref[T]] = v => KeyValue.of(v.ref)
  implicit def collection[Coll[T] <: Traversable[T], T: Encoder]: Encoder[Coll[T]] =
    coll =>
      ListValue.of {
        coll.toList.map(implicitly[Encoder[T]].encode(_)).asJava
      }
  implicit def optional[T: Encoder]: Encoder[Option[T]] = {
    case None        => NullValue.of()
    case Some(value) => implicitly[Encoder[T]].encode(value)
  }
}

trait Encoder_1 {

  /** generates a new `Encoder` for the type `T` */
  implicit def gen[T]: Encoder[T] = macro Magnolia.gen[T]
}
