package controllers

import play.api.mvc._
import java.io._
import io.Source

object Application extends Controller {

  def email(q: Option[String]) = Action {
    q match {
      case Some("Quelle est ton adresse email") => Ok("claire12.reynaud@laposte.net")
      case Some("Es tu abonne a la mailing list(OUI/NON)") => Ok("OUI")
      case Some("Es tu heureux de participer(OUI/NON)") => Ok("OUI")
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
    val source = Source.fromFile("/tmp/markdown")
    val lines = source.mkString
    source.close()
    Ok(lines)
  }

}