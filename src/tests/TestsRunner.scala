package mutatus.tests

import mutatus._
import probably._
import util.Try
import scala.util.Failure
import scala.util.Success

object TestsRunner extends TestApp {
  override def tests(): Unit = {
    MutatusSerializationSpec()
    // MutatusQueryBuilderSpec() //TODO restore after #7 query builder merge

    Try(System.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
      .map { _ =>
        if (Try(System.getenv("MUTATUS_TESTS_E2E"))
              .map(_.toUpperCase() == "TRUE")
              .getOrElse(false)) {
          println("Running E2E tests")
          val _ = MutatusE2ESpec()
        } else {
          println("Mutatus E2E tests disabled")
        }
      }
      .getOrElse(
        println("Google credentials not provided, E2E test skipped")
      )
  }

}
