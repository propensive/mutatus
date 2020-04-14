package mutatus.tests

import probably._
import util.Try

object TestsRunner extends TestApp {
  override def tests(): Unit = {
    SerializationSpec()
    QueryBuilderSpec()

    Try(System.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
      .map { _ =>
        if (Try(System.getenv("MUTATUS_TESTS_E2E"))
              .map(_.toUpperCase() == "TRUE")
              .getOrElse(false)) {

          println("Running E2E tests")
          val _ = EndToEndSpec()
        } else {
          println("Mutatus E2E tests disabled")
        }
      }
      .getOrElse(
        println("Google credentials not provided, E2E test skipped")
      )
  }

}
