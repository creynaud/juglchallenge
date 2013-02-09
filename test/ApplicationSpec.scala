package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import com.heeere._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
class ApplicationSpec extends Specification {
  
  "Application" should {
    "return OUI for /?q=As+tu+bien+recu+le+premier+enonce(OUI/NON)" in {
      running(FakeApplication()) {
        val home = route(FakeRequest(GET, "/?q=As+tu+bien+recu+le+premier+enonce(OUI/NON)")).get

        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/plain")
        contentAsString(home) must equalTo("OUI")
      }
    }
  }

}