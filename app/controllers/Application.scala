package controllers

import play.api.mvc._

object Application extends Controller {

  def email(q: Option[String]) = Action {
    q match {
      case Some("Quelle est ton adresse email") => Ok("claire12.reynaud@laposte.net")
      case _ => Ok("C'est pas faux")
    }
  }

}