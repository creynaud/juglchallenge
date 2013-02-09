package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

/**
 * add your integration spec here.
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class IntegrationSpec extends Specification {
  
  "Application" should {
    
    "work from within a browser" in {
      running(TestServer(3333), HTMLUNIT) { browser =>

        browser.goTo("http://localhost:3333/?q=Quelle+est+ton+adresse+email")

        browser.pageSource must contain("claire12.reynaud@laposte.net")

        browser.goTo("http://localhost:3333/?q=Es+tu+abonne+a+la+mailing+list(OUI/NON)")

        browser.pageSource must contain("OUI")

        browser.goTo("http://localhost:3333/?q=Es+tu+heureux+de+participer(OUI/NON)")

        browser.pageSource must contain("OUI")
      }
    }
    
  }
  
}