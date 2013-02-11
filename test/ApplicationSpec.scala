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
    "return OUI for /?q=As+tu+trouve+le+dernier+exercice+difficile(OUI/NON)" in {
      running(FakeApplication()) {
        val home = route(FakeRequest(GET, "/?q=As+tu+trouve+le+dernier+exercice+difficile(OUI/NON)")).get

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
        val json: String = "[{\"name\":\"coca-light\",\"value\":1},{\"name\":\"coca\",\"value\":138},{\"name\":\"au-travail-a-velo\",\"value\":-113},{\"name\":\"guitar-hero\",\"value\":-181}]"
        val solver = route(FakeRequest(POST, "/diet/resolve").withJsonBody(Json.parse(json))).get

        status(solver) must equalTo(OK)
        contentType(solver) must beSome.which(_ == "application/json")
        contentAsString(solver) must equalTo("[\"no solution\"]")
      }
    }
    "should not make request time out on POST /diet/resolve" in {
      running(FakeApplication()) {
        val json: String = "[{\"name\":\"coca\",\"value\":139},{\"name\":\"the-froid\",\"value\":122},{\"name\":\"vin\",\"value\":33},{\"name\":\"biere\",\"value\":129},{\"name\":\"coca-light\",\"value\":1},{\"name\":\"whisky\",\"value\":49},{\"name\":\"cafe\",\"value\":20},{\"name\":\"jus-de-fruit\",\"value\":51},{\"name\":\"limonade\",\"value\":133},{\"name\":\"cocktail\",\"value\":306},{\"name\":\"pain-baguette\",\"value\":285},{\"name\":\"pomme\",\"value\":110},{\"name\":\"yaourt\",\"value\":109},{\"name\":\"croissant\",\"value\":180},{\"name\":\"chips\",\"value\":570},{\"name\":\"hamburger\",\"value\":414},{\"name\":\"grand-coca\",\"value\":269},{\"name\":\"un-l-the-froid\",\"value\":332},{\"name\":\"deux-verres-vin\",\"value\":63},{\"name\":\"pinte-biere\",\"value\":279},{\"name\":\"un-l-coca-light\",\"value\":4},{\"name\":\"deux-verres-whisky\",\"value\":119},{\"name\":\"un-l-jus-de-fruit\",\"value\":224},{\"name\":\"un-l-limonade\",\"value\":333},{\"name\":\"un-grand-cocktail\",\"value\":530},{\"name\":\"pain-complet\",\"value\":385},{\"name\":\"un-grand-yaourt\",\"value\":209},{\"name\":\"croissant-au-beurre\",\"value\":180},{\"name\":\"un-grand-paquet-chips\",\"value\":1170},{\"name\":\"un-big-mac\",\"value\":419},{\"name\":\"une-heure-de-jogging\",\"value\":-1128},{\"name\":\"trois-quarts-heure-jogging\",\"value\":820},{\"name\":\"une-heure-seance-de-gym\",\"value\":-554},{\"name\":\"une-heure-roller\",\"value\":-323},{\"name\":\"une-heure-badminton\",\"value\":-330},{\"name\":\"une-heure-squash\",\"value\":-1220},{\"name\":\"une-heure-ping-pong\",\"value\":-203},{\"name\":\"une-heure-tennis\",\"value\":600},{\"name\":\"au-travail-a-velo\",\"value\":-117},{\"name\":\"au-travail-en-marchant\",\"value\":-214},{\"name\":\"trente-minutes-de-jogging\",\"value\":-528},{\"name\":\"an-travail-en-voiture\",\"value\":-21},{\"name\":\"seance-de-gym\",\"value\":-204},{\"name\":\"roller\",\"value\":-123},{\"name\":\"badminton\",\"value\":-87},{\"name\":\"squash\",\"value\":-660},{\"name\":\"partie-de-foot\",\"value\":-360},{\"name\":\"partie-de-foot-gardien\",\"value\":-55},{\"name\":\"ping-pong\",\"value\":-99},{\"name\":\"mario-kart\",\"value\":-120},{\"name\":\"guitar-hero\",\"value\":-182},{\"name\":\"wii-fit\",\"value\":-212},{\"name\":\"coder-pendant-quatre-heures\",\"value\":-430},{\"name\":\"sauna\",\"value\":-100},{\"name\":\"se-rendre-a-douze-seances-dans-la-journee\",\"value\":-232}]"
        val solver = route(FakeRequest(POST, "/diet/resolve").withJsonBody(Json.parse(json))).get

        status(solver) must equalTo(OK)
        contentType(solver) must beSome.which(_ == "application/json")
      }
    }
    "should not make request time out either on POST /diet/resolve" in {
      running(FakeApplication()) {
        val json: String = "[{\"name\":\"coca\",\"value\":139},{\"name\":\"the-froid\",\"value\":129},{\"name\":\"vin\",\"value\":34},{\"name\":\"biere\",\"value\":129},{\"name\":\"coca-light\",\"value\":1},{\"name\":\"whisky\",\"value\":49},{\"name\":\"cafe\",\"value\":20},{\"name\":\"jus-de-fruit\",\"value\":51},{\"name\":\"limonade\",\"value\":133},{\"name\":\"cocktail\",\"value\":306},{\"name\":\"pain-baguette\",\"value\":285},{\"name\":\"pomme\",\"value\":110},{\"name\":\"yaourt\",\"value\":109},{\"name\":\"croissant\",\"value\":180},{\"name\":\"chips\",\"value\":570},{\"name\":\"hamburger\",\"value\":414},{\"name\":\"grand-coca\",\"value\":269},{\"name\":\"un-l-the-froid\",\"value\":332},{\"name\":\"deux-verres-vin\",\"value\":63},{\"name\":\"pinte-biere\",\"value\":279},{\"name\":\"un-l-coca-light\",\"value\":4},{\"name\":\"deux-verres-whisky\",\"value\":119},{\"name\":\"un-l-jus-de-fruit\",\"value\":224},{\"name\":\"un-l-limonade\",\"value\":333},{\"name\":\"un-grand-cocktail\",\"value\":530},{\"name\":\"pain-complet\",\"value\":385},{\"name\":\"un-grand-yaourt\",\"value\":209},{\"name\":\"croissant-au-beurre\",\"value\":180},{\"name\":\"un-grand-paquet-chips\",\"value\":1170},{\"name\":\"un-big-mac\",\"value\":419},{\"name\":\"une-heure-de-jogging\",\"value\":-1128},{\"name\":\"trois-quarts-heure-jogging\",\"value\":820},{\"name\":\"une-heure-seance-de-gym\",\"value\":-554},{\"name\":\"une-heure-roller\",\"value\":-323},{\"name\":\"une-heure-badminton\",\"value\":-330},{\"name\":\"une-heure-squash\",\"value\":-1220},{\"name\":\"une-heure-ping-pong\",\"value\":-203},{\"name\":\"une-heure-tennis\",\"value\":600},{\"name\":\"au-travail-a-velo\",\"value\":-117},{\"name\":\"au-travail-en-marchant\",\"value\":-214},{\"name\":\"trente-minutes-de-jogging\",\"value\":-528},{\"name\":\"an-travail-en-voiture\",\"value\":-21},{\"name\":\"seance-de-gym\",\"value\":-204},{\"name\":\"roller\",\"value\":-123},{\"name\":\"badminton\",\"value\":-87},{\"name\":\"squash\",\"value\":-660},{\"name\":\"partie-de-foot\",\"value\":-360},{\"name\":\"partie-de-foot-gardien\",\"value\":-55},{\"name\":\"ping-pong\",\"value\":-99},{\"name\":\"mario-kart\",\"value\":-120},{\"name\":\"guitar-hero\",\"value\":-182},{\"name\":\"wii-fit\",\"value\":-212},{\"name\":\"coder-pendant-quatre-heures\",\"value\":-430},{\"name\":\"sauna\",\"value\":-100},{\"name\":\"se-rendre-a-douze-seances-dans-la-journee\",\"value\":-232},{\"name\":\"un-coca\",\"value\":140},{\"name\":\"un-the-froid\",\"value\":122},{\"name\":\"un-vin\",\"value\":33},{\"name\":\"un-biere\",\"value\":129},{\"name\":\"un-coca-light\",\"value\":1},{\"name\":\"un-whisky\",\"value\":49},{\"name\":\"un-cafe\",\"value\":20},{\"name\":\"un-jus-de-fruit\",\"value\":51},{\"name\":\"un-limonade\",\"value\":133},{\"name\":\"un-cocktail\",\"value\":306},{\"name\":\"un-pain-baguette\",\"value\":285},{\"name\":\"un-pomme\",\"value\":110},{\"name\":\"un-yaourt\",\"value\":166},{\"name\":\"un-croissant\",\"value\":180},{\"name\":\"un-chips\",\"value\":570},{\"name\":\"un-hamburger\",\"value\":417},{\"name\":\"un-grand-coca\",\"value\":269},{\"name\":\"un-un-l-the-froid\",\"value\":338},{\"name\":\"un-deux-verres-vin\",\"value\":72},{\"name\":\"un-pinte-biere\",\"value\":298},{\"name\":\"un-un-l-coca-light\",\"value\":5},{\"name\":\"un-deux-verres-whisky\",\"value\":120},{\"name\":\"un-un-l-jus-de-fruit\",\"value\":226},{\"name\":\"un-un-l-limonade\",\"value\":335},{\"name\":\"un-un-grand-cocktail\",\"value\":540},{\"name\":\"un-pain-complet\",\"value\":388},{\"name\":\"un-un-grand-yaourt\",\"value\":211},{\"name\":\"un-croissant-au-beurre\",\"value\":186},{\"name\":\"un-un-grand-paquet-chips\",\"value\":1177},{\"name\":\"un-un-big-mac\",\"value\":412},{\"name\":\"un-une-heure-de-jogging\",\"value\":-1178},{\"name\":\"un-trois-quarts-heure-jogging\",\"value\":825},{\"name\":\"un-une-heure-seance-de-gym\",\"value\":-557},{\"name\":\"un-une-heure-roller\",\"value\":-328},{\"name\":\"un-une-heure-badminton\",\"value\":-330},{\"name\":\"un-une-heure-squash\",\"value\":-1220},{\"name\":\"un-une-heure-ping-pong\",\"value\":-203},{\"name\":\"un-une-heure-tennis\",\"value\":600},{\"name\":\"un-au-travail-a-velo\",\"value\":-117},{\"name\":\"un-au-travail-en-marchant\",\"value\":-214},{\"name\":\"un-trente-minutes-de-jogging\",\"value\":-528},{\"name\":\"un-an-travail-en-voiture\",\"value\":-21},{\"name\":\"une-seance-de-gym\",\"value\":-204},{\"name\":\"un-roller\",\"value\":-123},{\"name\":\"un-badminton\",\"value\":-87},{\"name\":\"un-squash\",\"value\":-660},{\"name\":\"une-partie-de-foot\",\"value\":-360},{\"name\":\"une-partie-de-foot-gardien\",\"value\":-55},{\"name\":\"un-ping-pong\",\"value\":-99},{\"name\":\"un-mario-kart\",\"value\":-127},{\"name\":\"un-guitar-hero\",\"value\":-183},{\"name\":\"un-wii-fit\",\"value\":-215},{\"name\":\"un-coder-pendant-quatre-heures\",\"value\":-433},{\"name\":\"un-sauna\",\"value\":-101},{\"name\":\"un-se-rendre-a-douze-seances-dans-la-journee\",\"value\":-233}]"
        val solver = route(FakeRequest(POST, "/diet/resolve").withJsonBody(Json.parse(json))).get

        status(solver) must equalTo(OK)
        contentType(solver) must beSome.which(_ == "application/json")
      }
    }
  }

}