package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

/**
 * Tests for the application.
 */
class ApplicationSpec extends Specification {

  "Application" should {
    "return OUI for /?q=As+tu+bien+recu+le+deuxieme+enonce(OUI/NON)" in {
      running(FakeApplication()) {
        val home = route(FakeRequest(GET, "/?q=As+tu+bien+recu+le+deuxieme+enonce(OUI/NON)")).get

        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/plain")
        contentAsString(home) must equalTo("OUI")
      }
    }
    "solve the mine sweeper game on POST /minesweeper/resolve" in {
      running(FakeApplication()) {
        val solver = route(FakeRequest(POST, "/minesweeper/resolve", FakeHeaders(Seq(CONTENT_TYPE->Seq("text/plain"))), "4 4\n*...\n....\n.*..\n....")).get

        status(solver) must equalTo(OK)
        contentType(solver) must beSome.which(_ == "text/plain")
        contentAsString(solver) must equalTo("*100\n2210\n1*10\n1110")
      }
    }
  }

}