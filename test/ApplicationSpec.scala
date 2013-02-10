package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

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
    "solve the diet on POST /diet/resolve" in {
      running(FakeApplication()) {
        val json: String = "[{ \"name\" : \"coca-light\", \"value\" : 1 },{ \"name\" : \"croissant\", \"value\" : 180 },{ \"name\" : \"au-travail-a-velo\", \"value\" : -113 },{ \"name\" : \"guitar-hero\", \"value\" : -181 }]"
        val solver = route(FakeRequest(POST, "/diet/resolve").withJsonBody(Json.parse(json))).get

        status(solver) must equalTo(OK)
        contentType(solver) must beSome.which(_ == "application/json")
        contentAsString(solver) must contain("croissant")
        contentAsString(solver) must contain("coca-light")
        contentAsString(solver) must contain("guitar-hero")
        contentAsString(solver) must not contain("au-travail-a-velo")
      }
    }
    "no solution to the diet problem on POST /diet/resolve" in {
      running(FakeApplication()) {
        val json: String = "[{ \"name\" : \"coca-light\", \"value\" : 1 },{ \"name\" : \"croissant\", \"value\" : 180 },{ \"name\" : \"guitar-hero\", \"value\" : 181 }]"
        val solver = route(FakeRequest(POST, "/diet/resolve").withJsonBody(Json.parse(json))).get

        status(solver) must equalTo(OK)
        contentType(solver) must beSome.which(_ == "application/json")
        contentAsString(solver) must contain("no solution")
      }
    }
  }

}