package controllers

trait Vigenere {
  def encode(keyHex: String, messageText: String): String
  def decode(keyHex: String, cipherTextHex: String) : String
  def break(cipherTextHex: String): Option[BrokenCipher]
}

case class BrokenCipher(keyHex: String, messageText: String, info: List[(String, String)])

object VigenereCipher extends Vigenere {
  import StringHex._
  def encode(keyHex: String, messageText: String): String = xorByKey(keyHex, textToHex(messageText))
  def decode(keyHex: String, cipherTextHex: String) : String = hexToText(xorByKey(keyHex, cipherTextHex))

  def break(cipherTextHex: String): Option[BrokenCipher] = {
    //TODO
    val asciiCipher = hexToAscii(cipherTextHex).toList
    val keyLenFinder = KeyLengthFinder(asciiCipher)
    val disp = keyLenFinder.dispersion
    val dispStr = disp.map(k => k.length +":"+k.dispersion.toString).mkString("\r\n")


    val key = asciiToHex(keyLenFinder.byFrequencyLetters.map(_._2.headOption.getOrElse(0)))

    val message = decode(key, cipherTextHex)

    Some(BrokenCipher(key,message,List(
      ("Looking up to key size" -> keyLenFinder.maxKey.toString),
      ("Dispersion" -> dispStr),
      ("Key length" -> keyLenFinder.bestKey.toString),
      ("Available letters" -> keyLenFinder.availableLetters.map(x => x._1.toString+":"+x._2.map(_.toString).mkString(",")).mkString("\r\n")),
      ("By frequency" -> keyLenFinder.byFrequencyLetters.map(x => x._1.toString+":"+x._2.map(_.toString).mkString(",")).mkString("\r\n")),
      ("In hex" -> keyLenFinder.byFrequencyLetters.map(x => x._1.toString+":"+ x._2.map(n => Integer.toHexString(n)).mkString(",").toUpperCase).mkString("\r\n"))//,
      //("All possible keys" -> keyLenFinder.allDecartKeys.map(ks => asciiToHex(ks)).mkString("\r\n"))
    )))
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

  def asciiToHex: Iterable[Int] => String = _.map(n => Integer.toHexString(n)).mkString("").toUpperCase
}

case class KeyLengthFinder(asciiCipher: Iterable[Int]) {
  lazy val maxKey = Math.max(1, asciiCipher.size / 20)
  lazy val frequences: List[KeyLengthFrequency] = (1 to maxKey)
    .map(charFrequency _).toList

  lazy val dispersion = frequences.map(keyLenFreq => KeyDispersion(keyLenFreq.length, Math.sqrt(keyLenFreq.frequency.map(m =>
    Math.pow(m.size,2)).sum), keyLenFreq.frequency))
    .sortBy(_.dispersion)

  def charFrequency(keyLength: Int) = {
    val chars = asciiCipher.grouped(keyLength)
    val res = chars.foldLeft(0 until keyLength map(_ => Map[Int,Int]()))({
      case (freqList,b) => b.zipWithIndex.foldLeft(freqList)({
        case (freqListAcc,(asciiCharIndex,i)) =>
          val currentMap = freqListAcc(i)
          val newMap = currentMap + (asciiCharIndex -> (currentMap.getOrElse(asciiCharIndex, 0)+1))
          freqListAcc.updated(i, newMap)
      })
    })
    KeyLengthFrequency(keyLength, res.toList)
  }

  lazy val bestKey = dispersion.head.length

  private val highestFreq = List(32,65,97,69,101,79,111,84,116,73,105,83,115,78,110)

  lazy val availableLetters = (0 until bestKey).map(keyIndex => keyIndex -> availableLetter(keyIndex)).toList

  def availableLetter(keyIndex: Int) = {
    println(keyIndex)
    println(frequences.size)
    val freq = frequences.find(f => f.length==bestKey).get.frequency(keyIndex)
    (0 to 255).filter(keyAsciiChar => freq.forall(cipherChar => {
      val textChar = cipherChar._1 ^ keyAsciiChar
      textChar >= 32 && textChar <= 122
    })).toList
  }

  lazy val byFrequencyLetters = (0 until bestKey).map(keyIndex => keyIndex -> byFrequencyLetter(keyIndex)).toList

  def byFrequencyLetter(keyIndex: Int) = {
    val available = availableLetter(keyIndex)
    val freq = frequences.find(f => f.length==bestKey).get.frequency(keyIndex)
    val topFreq = freq.toList.sortBy(_._2).reverse.map(_._1).take(5)
    available.map(avChar => avChar -> topFreq.filter(tops => highestFreq.contains(tops ^ avChar)).size).filterNot(_._2 == 0).sortBy(_._2).reverse.map(_._1)
  }

  def decartKeys(depth: Int = 0, acc:List[List[Int]] = List(List())): List[List[Int]] = {
    if(depth == bestKey) acc
    else {
      val depthsLetters = byFrequencyLetters(depth)._2
      val newAcc = acc.flatMap(prevList => depthsLetters.map(letter => prevList :+ letter))
      //println("Depth " +depthsLetters.toString())
      //println("New acc " +newAcc.toString())
      decartKeys(depth + 1, newAcc)
    }
  }

  lazy val allDecartKeys = decartKeys()

}

case class KeyLengthFrequency(length: Int, frequency: List[Map[Int, Int]])

case class KeyDispersion(length: Int, dispersion: Double, frequency: List[Map[Int, Int]])