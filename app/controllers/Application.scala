package controllers

import play.api.mvc._

object Application extends Controller {

  def email(q: Option[String]) = Action {
    q match {
      case Some("Quelle est ton adresse email") => Ok("claire12.reynaud@laposte.net")
      case Some("Es tu abonne a la mailing list(OUI/NON)") => Ok("OUI")
      case Some("Es tu heureux de participer(OUI/NON)") => Ok("OUI")
      case _ => Ok("C'est pas faux")
    }
  }

}