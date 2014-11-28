package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action { request =>
    val query = request.body.asFormUrlEncoded.getOrElse(Map.empty)
    val key = query.get("key").flatMap(_.headOption)
    val message = query.get("message").flatMap(_.headOption)
    val cipher = key.flatMap(k => message.map(m => {
        m.foldLeft(("", 0))({
          case ((xs, i),b) =>
            val msgChar = Integer.parseInt(b.toString, 16)
            val keyChar = Integer.parseInt(k.charAt(i%k.size).toString, 16)
            val xor = Integer.toHexString(msgChar ^ keyChar)
            (xs ++ xor, i+1)
        })
    })).map(_._1.toUpperCase)
    Ok(views.html.index(key, message, cipher))
  }

}