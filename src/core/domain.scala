package mutatus

import quarantine.Domain

object Mutatus extends Domain[MutatusException] {
  implicit val mitigateHttp = antiphony.Http.mitigate(Mutatus) {
    case _: antiphony.NotAuthorized => Unathorized
    case ex                         => DatabaseException(ex)
  }

  implicit val mitigateEuphenismParser = euphemism.ParseException.mitigate(Mutatus) {
    case ex: euphemism.ParseException => SerializationException(ex.message)
  }

  implicit val mitigateEuphenismAccess = euphemism.Access.mitigate(Mutatus) {
    case ex => SerializationException(ex.getMessage())
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
