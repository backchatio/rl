package rl

import java.util.Locale

object Utils {

  private val toSkip = "!$&'()*+,;=:/?#\\[\\]@-._~".toSet

  object UrlEncoder extends (String => String) {
    def apply(toEncode: String) = {
      ((new StringBuilder /: toEncode.toBuffer) {
        (sb, c) =>
          if (c.isLetterOrDigit || toSkip.contains(c)) sb append c
          else { sb append '%' append c.toInt.toHexString.toUpperCase(Locale.ENGLISH) }
      }).toString()
    }
  }

  object UrlDecoder extends (String => String) {
    def apply(toDecode: String) = {
      val norm = toDecode.replace("+", "%20").replace("*", "%2A")
      val pctEncoded = """%([0123456789abcdefABCDEF][0123456789abcdefABCDEF]?)""".r
      val entities = pctEncoded.findAllIn(norm).toSet
      (norm /: entities) { (sb, enc) =>
        sb.replaceAllLiterally("%" + enc, Integer.parseInt(enc, 16).toChar.toString)
      }
    }
  }
}