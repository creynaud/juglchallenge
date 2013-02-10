package controllers

import play.api.mvc._
import java.io._
import io.Source

object Application extends Controller {

  def email(q: Option[String]) = Action {
    q match {
      case Some("As tu trouve le dernier exercice difficile(OUI/NON)") => Ok("OUI")
      case Some("Es tu pret a recevoir une enonce au format markdown par http post(OUI/NON)") => Ok("OUI")
      case _ => Ok("C'est pas faux")
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

  def resolve = Action {
    request =>
      val body: AnyContent = request.body
      val textBody: Option[String] = body.asText
      // Expecting text body
      textBody.map {
        text =>
          println(text)
          val solver: MineSweeperSolver = MineSweeperSolver.parse(text)
          val solution: String = solver.solution()
          println(solution)
          Ok(solution)
      }.getOrElse {
        BadRequest("Expecting text/plain request body")
      }
  }

}