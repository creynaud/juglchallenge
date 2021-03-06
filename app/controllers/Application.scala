package controllers

import play.api.mvc._
import java.io._
import io.Source
import model.{DietSolver, Activity, MineSweeperSolver}
import play.api.libs.json._

object Application extends Controller {

  def questions(q: Option[String]) = Action {
    q match {
      case Some("Veux tu tenter ta chance pour gagner un des prix(reserve aux membres du JUGL)(OUI/NON)") => Ok("OUI")
      case Some("Es tu pret a recevoir une enonce au format markdown par http post(OUI/NON)") => Ok("OUI")
      case _ => Ok("Hello, je suis le serveur de Claire pour le Challenge du JUGL !")
    }
  }

  def markdown = Action(parse.temporaryFile) {
    request =>
      val file: File = new File("/tmp/markdown")
      file.delete()
      request.body.moveTo(file)
      Ok("File uploaded")
  }

  def getMarkdown = Action {
    val source = Source.fromFile("/tmp/markdown")(io.Codec("UTF-8"))
    val lines = source.mkString
    source.close()
    Ok(lines)
  }

  def solveMineSweeper = Action(parse.text) {
    request =>
      val solver: MineSweeperSolver = MineSweeperSolver.parse(request.body)
      val solution: String = solver.solution()
      Ok(solution)
  }

  implicit val activityReads = new Reads[Activity] {
    def reads(js: JsValue): JsSuccess[Activity] = {
      JsSuccess[Activity](new Activity((js \ "name").as[String], (js \ "value").as[Int]))
    }
  }

  implicit val activityWrites = new Writes[Activity] {
    def writes(activity: Activity): JsValue = {
      JsString(activity.name)
    }
  }

  def solveDiet = Action(parse.json) {
    request =>
      val jsResult: JsResult[List[Activity]] = request.body.validate[List[Activity]]
      val solver: DietSolver = new DietSolver(jsResult.get)
      val solution: Set[Activity] = solver.solution()
      if (solution.size == 0) {
        Ok(Json.toJson(Json.arr("no solution")))
      } else {
        Ok(Json.toJson(solution.toList))
      }
  }

}