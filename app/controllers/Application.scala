package controllers

import play.api._
import play.api.mvc._


object Application extends Controller {

  abstract class EncodeRequest(request: Request[AnyContent]) {
    private def query = request.body.asFormUrlEncoded.getOrElse(Map.empty)
    private def p(n: String) = query.get(n).flatMap(_.headOption)
    private def key = p("key")
    def cipher = p("cipher")
    private def message = p("message")
    private def xor(input: Option[String]) = key.flatMap(k => input.map(m => xorByKey(k,m)))

    def encode = (key,message.map(_.map(m => Integer.toHexString(m.toInt)).mkString("")),xor(message))
    def decode = (key,xor(cipher).map(_.grouped(2).map(xs => Integer.parseInt(xs.toString(),16).toChar).map(_.toString).mkString("")),cipher)
    def break = {
      val km = cipher.map(breakCode _)
      (km.map(_._1),km.map(_._2),cipher)
    }
  }

  implicit def toEncodeRequest(request: Request[AnyContent]) = new EncodeRequest(request) {}

  def index = Action { request =>
    Ok((views.html.index.apply _).tupled(request.encode))
  }

  def decode = Action { request =>
    Ok((views.html.decode.apply _).tupled(request.decode))
  }

  def break = Action { request =>
    Ok((views.html.break.apply _).tupled(request.break))
  }
  
  private def xorByKey(key: String, input: String) = input.foldLeft(("", 0))({
        case ((xs, i),b) =>
          implicit def hexCharToInt(char:Char) = Integer.parseInt(char.toString, 16)
          val msgChar = Integer.parseInt(b.toString, 16)
          val keyChar = Integer.parseInt(key.charAt(i%key.size).toString, 16)
          val xor = Integer.toHexString(msgChar ^ keyChar)
          (xs ++ xor, i+1)
      })._1.toUpperCase

  def breakCode(cipher: String): (String, String) = {
    val charLength = 7
    val asciiCipher = cipher.grouped(2).map(twoBytes => Integer.parseInt(twoBytes, 16))
    val freq = charFrequency(charLength, asciiCipher)
    val str = (0 to 255).map(row => {
      (0 until charLength).map(col => {
        freq.apply(col).get(row).getOrElse(0)
      }).mkString(";")
    }).mkString("\r\n")
    val tryChars = freq.map(f => bestChars(f).map(o => Integer.toHexString(o)).mkString(";")).mkString("\r\n")
    ("",tryChars)
  }

  def charFrequency(keyLength: Int, asciiCipher: Iterator[Int]): List[Map[Int,Int]] = {
    val chars = asciiCipher.grouped(keyLength)
    val res = chars.foldLeft(0 until keyLength map(_ => Map[Int,Int]()))({
      case (freqList,b) => b.zipWithIndex.foldLeft(freqList)({
        case (freqListAcc,(asciiCharIndex,i)) =>
          val currentMap = freqListAcc(i)
          val newMap = currentMap + (asciiCharIndex -> (currentMap.getOrElse(asciiCharIndex, 0)+1))
          freqListAcc.updated(i, newMap)
      })
    })
    res.toList
  }

  def bestChars(freq: Map[Int,Int]) = {
    val xorOpts = (32 to 126).flatMap(asciiCharIndex => {
      val f = freq.map(x => (x._1 ^ asciiCharIndex))

      val min = f.min
      val max = f.max
      val delta = max - min
      println(min +" "+max)
      if(min >=32 && max <=126) {

        Some(asciiCharIndex)
      }
      else None
    })
    xorOpts
  }
}