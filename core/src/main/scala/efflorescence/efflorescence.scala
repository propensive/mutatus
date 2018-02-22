/* Efflorescence, version 0.10.0. Copyright 2018 Jon Pretty, Propensive Ltd.
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
package efflorescence

import magnolia._
import adversaria._
import com.google.cloud.datastore, datastore._
import util.Try
import language.experimental.macros, language.existentials, language.higherKinds
import annotation.StaticAnnotation
import collection.generic.CanBuildFrom

/** Efflorescence package object */
object `package` {
  /** provides a default instance of the GCP Datastore service */
  implicit val defaultDataStore: Service = Service(DatastoreOptions.getDefaultInstance.getService)

  /** provides `save` and `delete` methods on case class instances */
  implicit class DataExt[T <: Product](value: T) {
    /** saves the case class as a Datastore entity */
    def save()(implicit svc: Service, encoder: Encoder[T], dao: Dao[T], idField: IdField[T]): Ref[T] = {
      val keyValues = encoder.encode("", value)

      new Ref[T](svc.readWrite.put {
        val key = idField.idKey(idField.key(value)).newKey(dao.keyFactory)
        keyValues.foldLeft(Entity.newBuilder(key)) {
          case (entity, (key, dsType)) => dsType.set(entity, key)
        }.build()
      }.getKey)
    }

    /** deletes the Datastore entity with this ID */
    def delete()(implicit svc: Service, dao: Dao[T], idField: IdField[T]): Unit =
      svc.readWrite.delete(idField.idKey(idField.key(value)).newKey(dao.keyFactory))
  }

  private[efflorescence] def ifEmpty[T](str: String, empty: T, nonEmpty: String => T): T =
    if(str.isEmpty) empty else nonEmpty(str)
}

case class NotSavedException(kind: String) extends
    RuntimeException("entity of type $kind cannot be deleted becasue it has not been saved")

final class id() extends StaticAnnotation

/** a reference to another case class instance stored in the GCP Datastore */
case class Ref[T](ref: Key) {

  /** resolves the reference and returns a case class instance */
  def apply()(implicit svc: Service, decoder: Decoder[T]): T = decoder.decode(svc.read.get(ref))
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
object Geo { def apply(latLng: LatLng): Geo = Geo(latLng.getLatitude, latLng.getLongitude) }

/** a geographical position, with latitude and longitude */
case class Geo(lat: Double, lng: Double) { def toLatLng = LatLng.of(lat, lng) }

/** a representation of the GCP Datastore service */
case class Service(readWrite: Datastore) { def read: DatastoreReader = readWrite }

/** typeclass for encoding a value into a type which can be stored in the GCP Datastore */
trait Encoder[T] {
  def encode(key: String, value: T): List[(String, DsType)]
  def contraMap[T2](fn: T2 => T): Encoder[T2] = (k, v) => encode(k, fn(v))
}

/** typeclass for decoding a value from the GCP Datastore into a Scala type */
trait Decoder[T] {
  def decode(obj: BaseEntity[_], prefix: String = ""): T
  def map[T2](fn: T => T2): Decoder[T2] = (obj, p) => fn(decode(obj, p))
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

  implicit def annotationId[T, R](implicit ann: FindMetadataAux[T, R], implicitToId: ToId[R]): IdField[T] { type Return = ann.Return } =
    new IdField[T] {
      type Return = ann.Return
      def toId: ToId[Return] = implicitToId
      def key(t: T): Return = ann.get(t)
    }

  def from[T, R](fn: T => R)(implicit implicitToId: ToId[R]): IdField[T] { type Return = R } =
    new IdField[T] {
      type Return = R
      def toId: ToId[Return] = implicitToId
      def key(t: T): R = fn(t)
    }
}

object ToId {
  implicit val long: ToId[Long] = LongId(_)
  implicit val int: ToId[Int] = LongId(_)
  implicit val short: ToId[Short] = LongId(_)
  implicit val byte: ToId[Byte] = LongId(_)
  implicit val string: ToId[String] = StringId(_)
  implicit val guid: ToId[Guid] = guid => StringId(guid.guid)
  implicit val auto: ToId[Auto] = v => LongId(v.id)
}

trait ToId[R] { def toId(value: R): IdKey  }

case class Auto(id: Long = -1) extends AnyVal {
  override def toString = id.toString
}

sealed trait IdKey { def newKey(keyFactory: KeyFactory): Key }

case class StringId(id: String) extends IdKey {
  def newKey(kf: KeyFactory): Key = kf.newKey(id)
}

object Guid {
  def apply(str: String): Guid =
    new Guid(if(str == "") java.util.UUID.randomUUID().toString else str)

  def apply(): Guid = apply("")
}

class Guid(val guid: String) extends IdKey {
  def newKey(kf: KeyFactory): Key = kf.newKey(guid)
  override def hashCode = guid.hashCode
  override def equals(that: Any) = that match {
    case that: Guid => that.guid == guid
    case _ => false
  }
  override def toString = guid
}

case class LongId(id: Long) extends IdKey {
  def newKey(kf: KeyFactory): Key = kf.newKey(id)
}

/** a data access object for a particular type */
case class Dao[T](kind: String)(implicit svc: Service, namespace: Namespace) {

  private[efflorescence] lazy val keyFactory = {
    val baseFactory = svc.readWrite.newKeyFactory().setKind(kind)
    namespace.option.foldLeft(baseFactory)(_.setNamespace(_))
  }

  /** returns an iterator of all the values of this type stored in the GCP Platform */
  def all()(implicit decoder: Decoder[T]): Iterator[T] = {
    val baseQueryBuilder = Query.newEntityQueryBuilder().setKind(kind)
    val query: EntityQuery = namespace.option.foldLeft(baseQueryBuilder)(_.setNamespace(_)).build()
    val results: QueryResults[Entity] = svc.read.run(query)

    new Iterator[Entity] {
      def next(): Entity = results.next()
      def hasNext: Boolean = results.hasNext
    }.map(decoder.decode(_))
  }

  def unapply[R](id: R)(implicit decoder: Decoder[T], idField: IdField[T] { type Return = R }): Option[T] = {
    val key = idField.idKey(id).newKey(keyFactory)
    Try(decoder.decode(svc.read.get(key))).toOption
  }
}

/** generic type for Datastore datatypes */
class DsType(val set: (Entity.Builder, String) => Entity.Builder)

/** companion object for DsType */
object DsType {
  final case class DsString(value: String) extends DsType(_.set(_, value))
  final case class DsLong(value: Long) extends DsType(_.set(_, value))
  final case class DsBoolean(value: Boolean) extends DsType(_.set(_, value))
  final case class DsDouble(value: Double) extends DsType(_.set(_, value))
  final case class DsKey(value: Ref[_]) extends DsType(_.set(_, value.ref))
  final case class DsLatLng(value: Geo) extends DsType(_.set(_, value.toLatLng))
  final case object DsRemove extends DsType(_.remove(_))
}

/** companion object for `Decoder`, including Magnolia generic derivation */
object Decoder extends Decoder_1 {
  type Typeclass[T] = Decoder[T]

  /** combines `Decoder`s for each parameter of the case class `T` into a `Decoder` for `T` */
  def combine[T](caseClass: CaseClass[Decoder, T]): Decoder[T] = (obj, prefix) =>
    caseClass.construct { param =>
      param.typeclass.decode(obj, ifEmpty(prefix, param.label, _+s".${param.label}"))
    }

  /** tries `Decoder`s for each subtype of sealed trait `T` until one doesn`t throw an exception
   *
   *  This is a suboptimal implementation, and a better solution may be possible with more work. */
  def dispatch[T](st: SealedTrait[Decoder, T]): Decoder[T] = (obj, prefix) =>
    st.subtypes.view.map { sub => Try(sub.typeclass.decode(obj, prefix)) }.find(_.isSuccess).get.get

  implicit val int: Decoder[Int] = _.getLong(_).toInt
  implicit val string: Decoder[String] = _.getString(_)
  implicit val guid: Decoder[Guid] = string.map(Guid(_))
  implicit val long: Decoder[Long] = _.getLong(_)
  implicit val byte: Decoder[Byte] = _.getLong(_).toByte
  implicit val short: Decoder[Short] = _.getLong(_).toShort
  implicit val char: Decoder[Char] = _.getString(_).head
  implicit val boolean: Decoder[Boolean] = _.getBoolean(_)
  implicit val double: Decoder[Double] = _.getDouble(_)
  implicit val float: Decoder[Float] = _.getDouble(_).toFloat
  implicit val geo: Decoder[Geo] = (obj, name) => Geo(obj.getLatLng(name))
  implicit def ref[T](implicit idField: IdField[T]): Decoder[Ref[T]] = (obj, ref) => Ref[T](obj.getKey(ref))
  
  implicit def optional[T: Decoder]: Decoder[Option[T]] =
    (obj, key) => if(obj.contains(key)) Some(implicitly[Decoder[T]].decode(obj, key)) else None
  
  implicit def collection[Coll[T] <: Traversable[T], T: Decoder](implicit cbf: CanBuildFrom[Nothing, T, Coll[T]]):
      Decoder[Coll[T]] = new Decoder[Coll[T]] {
    def decode(obj: BaseEntity[_], prefix: String): Coll[T] = {
      Stream.from(0).map { idx =>
        Try(implicitly[Decoder[T]].decode(obj, s"$prefix.$idx"))
      }.takeWhile(_.isSuccess).map(_.get).to[Coll]
    }
  }
}

trait Decoder_1 {
  /** generates a new `Decoder` for the type `T` */
  implicit def gen[T]: Decoder[T] = macro Magnolia.gen[T]
}

/** companion object for `Encoder`, including Magnolia generic derivation */
object Encoder extends Encoder_1 {
  type Typeclass[T] = Encoder[T]
  
  /** combines `Encoder`s for each parameter of the case class `T` into a `Encoder` for `T` */
  def combine[T](caseClass: CaseClass[Encoder, T]): Encoder[T] = { (key, value) =>
    caseClass.parameters.to[List].flatMap { param =>
      param.typeclass.encode(param.label, param.dereference(value)) map {
        case (k, v) => (ifEmpty(key, k, _+s".$k"), v)
      }
    }
  }

  /** chooses the appropriate `Encoder` of a subtype of the sealed trait `T` based on its type */
  def dispatch[T](sealedTrait: SealedTrait[Encoder, T]): Encoder[T] =
    (key, value) => sealedTrait.dispatch(value) { st => st.typeclass.encode(key, st.cast(value)) }

  implicit val string: Encoder[String] = (k, v) => List((k, DsType.DsString(v)))
  implicit val guid: Encoder[Guid] = string.contraMap[Guid](_.guid)
  implicit val long: Encoder[Long] = (k, v) => List((k, DsType.DsLong(v)))
  implicit val int: Encoder[Int] = (k, v) => List((k, DsType.DsLong(v)))
  implicit val short: Encoder[Short] = (k, v) => List((k, DsType.DsLong(v)))
  implicit val char: Encoder[Char] = (k, v) => List((k, DsType.DsString(v.toString)))
  implicit val byte: Encoder[Byte] = (k, v) => List((k, DsType.DsLong(v)))
  implicit val boolean: Encoder[Boolean] = (k, v) => List((k, DsType.DsBoolean(v)))
  implicit val double: Encoder[Double] = (k, v) => List((k, DsType.DsDouble(v)))
  implicit val float: Encoder[Float] = (k, v) => List((k, DsType.DsDouble(v)))
  implicit val geo: Encoder[Geo] = (k, v) => List((k, DsType.DsLatLng(v)))
  implicit def ref[T]: Encoder[Ref[T]] = (k, v) => List((k, DsType.DsKey(v)))

  implicit def optional[T: Encoder]: Encoder[Option[T]] = (k, opt) => opt match {
    case None => List((k, DsType.DsRemove))
    case Some(value) => implicitly[Encoder[T]].encode(k, value)
  }
  
  implicit def collection[Coll[T] <: Traversable[T], T: Encoder]: Encoder[Coll[T]] = (prefix, coll) =>
    coll.to[List].zipWithIndex.flatMap { case (t, idx) =>
      implicitly[Encoder[T]].encode(ifEmpty(prefix, s"$idx", _+s".$idx"), t)
    }
}

trait Encoder_1 {
  /** generates a new `Encoder` for the type `T` */
  implicit def gen[T]: Encoder[T] = macro Magnolia.gen[T]
}

/** companion object for data access objects */
object Dao {
  implicit def apply[T](implicit metadata: TypeMetadata[T], namespace: Namespace): Dao[T] =
    Dao(metadata.typeName)
}
