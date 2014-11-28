package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action { request =>
    val query = request.body.asFormUrlEncoded.getOrElse(Map.empty)
    val key = query.get("key").flatMap(_.headOption)
    val message = query.get("message").flatMap(_.headOption)
    val cipher = key.flatMap(k => message.map(m => xor(k,m)))
    Ok(views.html.index(key, message, cipher))
  }

  def decode = Action { request =>
    val query = request.body.asFormUrlEncoded.getOrElse(Map.empty)
    val key = query.get("key").flatMap(_.headOption)
    val cipher = query.get("cipher").flatMap(_.headOption)
    val message = key.flatMap(k => cipher.map(c => xor(k,c)))
    Ok(views.html.decode(key, message, cipher))
  }
  
  private def xor(key: String, input: String) = {
      input.foldLeft(("", 0))({
        case ((xs, i),b) =>
          val msgChar = Integer.parseInt(b.toString, 16)
          val keyChar = Integer.parseInt(key.charAt(i%key.size).toString, 16)
          val xor = Integer.toHexString(msgChar ^ keyChar)
          (xs ++ xor, i+1)
      })._1.toUpperCase
  }

}