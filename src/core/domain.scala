package mutatus

import quarantine.Domain
import euphemism.DeserializationException
import euphemism.IndexNotFound
import euphemism.LabelNotFound
import euphemism.UnexpectedType

object Mutatus extends Domain[MutatusException] {
  implicit val mitigateHttp = antiphony.Http.mitigate(Mutatus) {
    case _: antiphony.NotAuthorized => Unathorized
    case ex                         => DatabaseException(ex)
  }

  implicit val mitigateEuphenismParser = euphemism.ParseException.mitigate(Mutatus) {
    case ex: euphemism.ParseException => SerializationException(s"Failed to parse json @ ${ex.line}/${ex.column} - ${ex.message}")
  }

  implicit val mitigateEuphenismAccess = euphemism.Access.mitigate(Mutatus) {
    case e @ DeserializationException() => SerializationException(s"Failed to deserialize json, ${e.getMessage()}")
    case IndexNotFound(index) => SerializationException(s"Failed to find index $index")
    case LabelNotFound(label) => SerializationException(s"Failed to find label $label")
    case UnexpectedType(expectedType) => SerializationException(s"Unexpected type, expected $expectedType")
  }
}

sealed abstract class MutatusException(msg: String) extends Exception(msg)
case class SerializationException(msg: String) extends MutatusException(msg)
case class DatabaseException(exception: Exception) extends MutatusException(exception.getMessage())
case class NotSavedException(kind: String) extends MutatusException(s"Entity of type $kind cannot be deleted becasue it has not been saved")

case class InvalidIndexDefinition(index: SchemaDef.IndexDef[_])
    extends MutatusException((s"Index def with id ${index.indexId} for kind ${index.kind} contains errors"))

case object IndexCreationTimeout extends MutatusException(s"Timeout for index creation exceeded")

case object Unathorized extends MutatusException("Provided credentials were not sufficient to perform Datastore operation")
case class  InactiveTransactionException(tx: com.google.cloud.datastore.Transaction) extends MutatusException(s"Transaction ${tx.getTransactionId()} was already commited or timeout out")
