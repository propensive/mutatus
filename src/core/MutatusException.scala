package mutatus

sealed abstract class MutatusException(msg: String) extends Exception(msg)
case class SerializationException(msg: String) extends MutatusException(msg)
case class DatabaseException(exception: Exception)
    extends MutatusException(exception.getMessage())
case class NotSavedException(kind: String)
    extends MutatusException(
      s"Entity of type $kind cannot be deleted becasue it has not been saved"
    )
case object Unathorized extends MutatusException("Provided credentials were not sufficient to perform Datastore operation")

sealed trait IndexValidationException{
  self: MutatusException => 
}

case class InvalidIndexDefinition(index: SchemaDef.IndexDef[_]) 
  extends MutatusException((s"Index def with id ${index.indexId} for kind ${index.kind} contains errors")) 
  with IndexValidationException

case object IndexCreationTimeout 
  extends MutatusException(s"Timeout for index creation exceeded") 
  with IndexValidationException 