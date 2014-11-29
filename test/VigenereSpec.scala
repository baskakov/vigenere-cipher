import controllers.{Vigenere, VigenereCipher}
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class VigenereSpec extends Specification {

  import controllers.StringHex._

  val cipher: Vigenere = VigenereCipher

  val keyHex = "01234ABCDF"

  val message = "This life, which had been the tomb of his virtue and of his honour, is but a walking shadow; a poor player, that struts and frets his hour upon the stage, and then is heard no more: it is a tale told by an idiot, full of sound and fury, signifying nothing."

  "Encoder" should {
    "Return same message on zero key" in {
      cipher.encode("000000",message) must_== textToHex(message)
    }

    "Return same message on decode / encode" in {
      val enc = cipher.encode(keyHex, message)
      val dec = cipher.decode(keyHex, enc)
      dec must_== message
    }
  }

  "Breaker" should {
    "Find correct key length" in {

    }
  }
}
