package efflorescence

import magnolia._
import com.google.cloud.datastore._
import util.Try
import language.experimental.macros, language.existentials

object `package` {
  implicit val defaultDataStore: Service = Service(DatastoreOptions.getDefaultInstance.getService)

  implicit class DataExt[T <: Product](value: T) {
    def save()(implicit svc: Service, encoder: Encoder[T], id: Id[T], dao: Dao[T]): Ref[T] = {
      val DsType.DsObject(keyValues) = encoder.encode(value)

      new Ref[T](svc.readWrite.put {
        keyValues
          .foldLeft(Entity.newBuilder(dao.keyFactory.newKey(id.key(value)))) {
            case (entity, (k, dsType: DsSimpleType)) => dsType.set(entity, k)
          }
          .build()
      }.getKey)
    }

    def delete()(implicit svc: Service, id: Id[T], dao: Dao[T]): Unit =
      svc.readWrite.delete(dao.keyFactory.newKey(id.key(value)))
  }
}

case class Ref[T](ref: Key) {
  def apply()(implicit svc: Service, decoder: Decoder[T]): T = decoder.decode(svc.read.get(ref))
  override def toString: String = s"$Ref[${ref.getKind}]($key)"
  def key: String = ref.getNameOrId.toString
}

object Geo { def apply(latLng: LatLng): Geo = Geo(latLng.getLatitude, latLng.getLongitude) }
case class Geo(lat: Double, lng: Double) { def toLatLng = LatLng.of(lat, lng) }

case class Service(readWrite: Datastore) { def read: DatastoreReader = readWrite }
trait Encoder[T] { def encode(t: T): DsType }
trait Decoder[T] { def decode(obj: BaseEntity[_], prefix: String = ""): T }

trait Id[T] { def key(t: T): String }

case class Dao[T](kind: String)(implicit svc: Service) {

  private[efflorescence] lazy val keyFactory = svc.readWrite.newKeyFactory().setKind(kind)

  def all()(implicit decoder: Decoder[T]): Iterator[T] = {
    val query = Query.newEntityQueryBuilder().setKind(kind).build()
    val results: QueryResults[Entity] = svc.read.run(query)

    new Iterator[Entity] {
      def next(): Entity = results.next()
      def hasNext: Boolean = results.hasNext
    }.map(decoder.decode(_))
  }
}

sealed trait DsType
class DsSimpleType(val set: (Entity.Builder, String) => Entity.Builder) extends DsType

object DsType {
  final case class DsString(value: String) extends DsSimpleType(_.set(_, value))
  final case class DsLong(value: Long) extends DsSimpleType(_.set(_, value))
  final case class DsBoolean(value: Boolean) extends DsSimpleType(_.set(_, value))
  final case class DsDouble(value: Double) extends DsSimpleType(_.set(_, value))
  final case class DsKey(value: Ref[_]) extends DsSimpleType(_.set(_, value.ref))
  final case class DsLatLng(value: Geo) extends DsSimpleType(_.set(_, value.toLatLng))
  final case class DsObject(keyValues: Map[String, DsSimpleType]) extends DsType
}

object Decoder {
  type Typeclass[T] = Decoder[T]
  implicit def gen[T]: Decoder[T] = macro Magnolia.gen[T]

  def combine[T](caseClass: CaseClass[Decoder, T]): Decoder[T] = (obj, prefix) =>
    caseClass.construct { param =>
      param.typeclass.decode(obj, if (prefix.isEmpty) param.label else s"$prefix.${param.label}")
    }

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
  implicit def ref[T: Dao: Id]: Decoder[Ref[T]] = (obj, ref) => Ref[T](obj.getKey(ref))
}

object Encoder {
  type Typeclass[T] = Encoder[T]
  implicit def gen[T]: Encoder[T] = macro Magnolia.gen[T]

  def combine[T](caseClass: CaseClass[Encoder, T]): Encoder[T] = value => DsType.DsObject {
    caseClass.parameters.flatMap { param =>
      param.typeclass.encode(param.dereference(value)) match {
        case value: DsSimpleType => List(param.label -> value)
        case DsType.DsObject(elems) => elems.map { case (k, v) => (s"${param.label}.$k", v) }
      }
    }.toMap
  }

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

object Dao extends Dao_1 {
  type Typeclass[T] = Dao[T]
  implicit def apply[T <: Product]: Dao[T] = macro Magnolia.gen[T]
  def combine[T: Decoder](caseClass: CaseClass[Dao, T]): Dao[T] = Dao(caseClass.typeName)
  def dispatch[T](sealedTrait: SealedTrait[Dao, T]): Dao[T] = ???
}

trait Dao_1 { implicit def parameters[T]: Dao[T] = Dao("") }
