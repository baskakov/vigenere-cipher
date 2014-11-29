package controllers

trait Vigenere {
  def encode(keyHex: String, messageText: String): String
  def decode(keyHex: String, cipherTextHex: String) : String
  def break(cipherTextHex: String): Option[BrokenCipher]
}

case class BrokenCipher(keyHex: String, messageText: String)

object VigenereCipher extends Vigenere {
  import StringHex._
  def encode(keyHex: String, messageText: String): String = xorByKey(keyHex, textToHex(messageText))
  def decode(keyHex: String, cipherTextHex: String) : String = hexToText(xorByKey(keyHex, cipherTextHex))

  def break(cipherTextHex: String): Option[BrokenCipher] = {
    //TODO


    //val (keyLength: Int, frequences: Map[Int, Int]) = ???
    None
  }

  private def charFrequency(keyLength: Int, asciiCipher: Iterable[Int]): List[Map[Int,Int]] = {
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
}

object StringHex {
  def xorByKey(keyHex: String, inputHex: String) = inputHex.foldLeft(("", 0))({
    case ((xs, i),b) =>
      implicit def hexCharToInt(char:Char) = Integer.parseInt(char.toString, 16)
      val msgChar = Integer.parseInt(b.toString, 16)
      val keyChar = Integer.parseInt(keyHex.charAt(i%keyHex.size).toString, 16)
      val xor = Integer.toHexString(msgChar ^ keyChar)
      (xs ++ xor, i+1)
  })._1.toUpperCase
  
  def textToHex: String => String = _.map(c => Integer.toHexString(c.toInt)).mkString("").toUpperCase

  def hexToText: String => String = hexToAscii.andThen(_.map(_.toChar.toString).mkString(""))

  def hexToAscii: String => Iterator[Int] = _.grouped(2).map(twoBytes => Integer.parseInt(twoBytes, 16))
}
