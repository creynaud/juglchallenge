package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
class ApplicationSpec extends Specification {
  
  "Application" should {

    "return my email for /?q=Quelle+est+ton+adresse+email" in {
      running(FakeApplication()) {
        val home = route(FakeRequest(GET, "/?q=Quelle+est+ton+adresse+email")).get

        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/plain")
        contentAsString(home) must equalTo("claire12.reynaud@laposte.net")
      }
    }

    "return OUI for /?q=Es+tu+abonne+a+la+mailing+list(OUI/NON)" in {
      running(FakeApplication()) {
        val home = route(FakeRequest(GET, "/?q=Es+tu+abonne+a+la+mailing+list(OUI/NON)")).get

        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/plain")
        contentAsString(home) must equalTo("OUI")
      }
    }

    "return OUI for /?q=Es+tu+heureux+de+participer(OUI/NON)" in {
      running(FakeApplication()) {
        val home = route(FakeRequest(GET, "/?q=Es+tu+heureux+de+participer(OUI/NON)")).get

        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/plain")
        contentAsString(home) must equalTo("OUI")
      }
    }
  }
}