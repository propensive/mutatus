package efflorescence

import magnolia._
import adversaria._
import com.google.cloud.datastore._
import util.Try
import language.experimental.macros, language.existentials
import annotation.StaticAnnotation

/** Efflorescence package object */
object `package` {
  /** provides a default instance of the GCP Datastore service */
  implicit val defaultDataStore: Service = Service(DatastoreOptions.getDefaultInstance.getService)

  /** provides `save` and `delete` methods on case class instances */
  implicit class DataExt[T <: Product](value: T) {
    /** saves the case class as a Datastore entity */
    def save()(implicit svc: Service, encoder: Encoder[T], id: IdField[T], dao: Dao[T]): Ref[T] = {
      val DsType.DsObject(keyValues) = encoder.encode(value)

      new Ref[T](svc.readWrite.put {
        keyValues
          .foldLeft(Entity.newBuilder(dao.keyFactory.newKey(id.key(value)))) {
            case (entity, (k, dsType: DsSimpleType)) => dsType.set(entity, k)
          }
          .build()
      }.getKey)
    }

    /** deletes the Datastore entity with this ID */
    def delete()(implicit svc: Service, id: IdField[T], dao: Dao[T]): Unit =
      svc.readWrite.delete(dao.keyFactory.newKey(id.key(value)))
  }
}

final class id() extends StaticAnnotation

/** a reference to another case class instance stored in the GCP Datastore */
case class Ref[T](ref: Key) {

  /** resolves the reference and returns a case class instance */
  def apply()(implicit svc: Service, decoder: Decoder[T]): T = decoder.decode(svc.read.get(ref))
  override def toString: String = s"$Ref[${ref.getKind}]($key)"

  /** a `String` version of the key contained by this reference */
  def key: String = ref.getNameOrId.toString
}

/** companion object for Geo instances */
object Geo { def apply(latLng: LatLng): Geo = Geo(latLng.getLatitude, latLng.getLongitude) }

/** a geographical position, with latitude and longitude */
case class Geo(lat: Double, lng: Double) { def toLatLng = LatLng.of(lat, lng) }

/** a representation of the GCP Datastore service */
case class Service(readWrite: Datastore) { def read: DatastoreReader = readWrite }

/** typeclass for encoding a value into a type which can be stored in the GCP Datastore */
trait Encoder[T] { def encode(t: T): DsType }

/** typeclass for decoding a value from the GCP Datastore into a Scala type */
trait Decoder[T] { def decode(obj: BaseEntity[_], prefix: String = ""): T }

/** typeclass for generating an ID field from a case class */
trait IdField[T] { def key(t: T): String }

object IdField {
  implicit def annotationId[T](implicit ann: FindMetadata[id, T]): IdField[T] =
    new IdField[T] { def key(t: T): String = ann.get(t).toString }
}

/** a data access object for a particular type */
case class Dao[T](kind: String)(implicit svc: Service) {

  private[efflorescence] lazy val keyFactory = svc.readWrite.newKeyFactory().setKind(kind)

  /** returns an iterator of all the values of this type stored in the GCP Platform */
  def all()(implicit decoder: Decoder[T]): Iterator[T] = {
    val query: EntityQuery = Query.newEntityQueryBuilder().setKind(kind).build()
    val results: QueryResults[Entity] = svc.read.run(query)

    new Iterator[Entity] {
      def next(): Entity = results.next()
      def hasNext: Boolean = results.hasNext
    }.map(decoder.decode(_))
  }
}

/** generic type for Datastore datatypes */
sealed trait DsType

/** generic type for simple Datastore datatypes, which have no complex structure */
class DsSimpleType(val set: (Entity.Builder, String) => Entity.Builder) extends DsType

/** companion object for DsType */
object DsType {
  final case class DsString(value: String) extends DsSimpleType(_.set(_, value))
  final case class DsLong(value: Long) extends DsSimpleType(_.set(_, value))
  final case class DsBoolean(value: Boolean) extends DsSimpleType(_.set(_, value))
  final case class DsDouble(value: Double) extends DsSimpleType(_.set(_, value))
  final case class DsKey(value: Ref[_]) extends DsSimpleType(_.set(_, value.ref))
  final case class DsLatLng(value: Geo) extends DsSimpleType(_.set(_, value.toLatLng))
  final case class DsObject(keyValues: Map[String, DsSimpleType]) extends DsType
}

/** companion object for `Decoder`, including Magnolia generic derivation */
object Decoder {
  type Typeclass[T] = Decoder[T]
  
  /** generates a new `Decoder` for the type `T` */
  implicit def gen[T]: Decoder[T] = macro Magnolia.gen[T]

  /** combines `Decoder`s for each parameter of the case class `T` into a `Decoder` for `T` */
  def combine[T](caseClass: CaseClass[Decoder, T]): Decoder[T] = (obj, prefix) =>
    caseClass.construct { param =>
      param.typeclass.decode(obj, if (prefix.isEmpty) param.label else s"$prefix.${param.label}")
    }

  /** tries `Decoder`s for each subtype of sealed trait `T` until one doesn`t throw an exception
   *
   *  This is a suboptimal implementation, and a better solution may be possible with more work. */
  def dispatch[T](st: SealedTrait[Decoder, T]): Decoder[T] = (obj, prefix) =>
    st.subtypes.view.map { sub => Try(sub.typeclass.decode(obj, prefix)) }.find(_.isSuccess).get.get

  implicit val int: Decoder[Int] = _.getLong(_).toInt
  implicit val string: Decoder[String] = _.getString(_)
  implicit val long: Decoder[Long] = _.getLong(_)
  implicit val byte: Decoder[Byte] = _.getLong(_).toByte
  implicit val short: Decoder[Short] = _.getLong(_).toShort
  implicit val char: Decoder[Char] = _.getString(_).head
  implicit val boolean: Decoder[Boolean] = _.getBoolean(_)
  implicit val double: Decoder[Double] = _.getDouble(_)
  implicit val float: Decoder[Float] = _.getDouble(_).toFloat
  implicit val geo: Decoder[Geo] = (obj, name) => Geo(obj.getLatLng(name))
  implicit def ref[T: Dao: IdField]: Decoder[Ref[T]] = (obj, ref) => Ref[T](obj.getKey(ref))
}

/** companion object for `Encoder`, including Magnolia generic derivation */
object Encoder {
  type Typeclass[T] = Encoder[T]
  
  /** generates a new `Encoder` for the type `T` */
  implicit def gen[T]: Encoder[T] = macro Magnolia.gen[T]

  /** combines `Encoder`s for each parameter of the case class `T` into a `Encoder` for `T` */
  def combine[T](caseClass: CaseClass[Encoder, T]): Encoder[T] = value => DsType.DsObject {
    caseClass.parameters.flatMap { param =>
      param.typeclass.encode(param.dereference(value)) match {
        case value: DsSimpleType => List(param.label -> value)
        case DsType.DsObject(elems) => elems.map { case (k, v) => (s"${param.label}.$k", v) }
      }
    }.toMap
  }

  /** chooses the appropriate `Encoder` of a subtype of the sealed trait `T` based on its type */
  def dispatch[T](sealedTrait: SealedTrait[Encoder, T]): Encoder[T] =
    value => sealedTrait.dispatch(value) { st => st.typeclass.encode(st.cast(value)) }

  implicit val string: Encoder[String] = DsType.DsString(_)
  implicit val long: Encoder[Long] = DsType.DsLong(_)
  implicit val int: Encoder[Int] = DsType.DsLong(_)
  implicit val short: Encoder[Short] = DsType.DsLong(_)
  implicit val char: Encoder[Char] = c => DsType.DsString(c.toString)
  implicit val byte: Encoder[Byte] = DsType.DsLong(_)
  implicit val boolean: Encoder[Boolean] = DsType.DsBoolean(_)
  implicit val double: Encoder[Double] = DsType.DsDouble(_)
  implicit val float: Encoder[Float] = DsType.DsDouble(_)
  implicit val geo: Encoder[Geo] = DsType.DsLatLng(_)
  implicit def ref[T]: Encoder[Ref[T]] = DsType.DsKey(_)
}

object Dao {
  implicit def apply[T](implicit metadata: TypeMetadata[T]): Dao[T] = Dao(metadata.typeName)
}
