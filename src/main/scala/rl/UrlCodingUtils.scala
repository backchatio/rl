package rl

import java.util.Locale
import util.matching.Regex
import util.matching.Regex.Match
import java.nio.charset.Charset
import java.nio.{ CharBuffer, ByteBuffer }
import collection.immutable.BitSet

trait UrlCodingUtils {

  private val toSkip = BitSet((('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "!$&'()*+,;=:/?#[]@-._~".toSet).map(_.toInt): _*)

  private[rl] val PctEncoded = """%([0-9a-fA-F][0-9a-fA-F])""".r
  private val LowerPctEncoded = """%([0-9a-f][0-9a-f])""".r
  private val InvalidChars = "[^\\.a-zA-Z0-9!$&'()*+,;=:/?#\\[\\]@-_~]".r

  private val HexUpperCaseChars = (0 until 16) map { i ⇒ Character.toUpperCase(Character.forDigit(i, 16)) }

  private[rl] val UTF_8 = "UTF-8".intern
  private[rl] val Utf8 = Charset.forName(UTF_8)

  def isUrlEncoded(string: String) = {
    PctEncoded.findFirstIn(string).isDefined
  }

  def containsInvalidUriChars(string: String) = {
    InvalidChars.findFirstIn(string).isDefined
  }

  def needsUrlEncoding(string: String) = {
    !isUrlEncoded(string) && containsInvalidUriChars(string)
  }

  def ensureUrlEncoding(string: String) = if (needsUrlEncoding(string)) urlEncode(string) else string

  def ensureUppercasedEncodings(string: String) = {
    LowerPctEncoded.replaceAllIn(string, (_: Match) match {
      case Regex.Groups(v) ⇒ "%" + v.toUpperCase(Locale.ENGLISH)
    })
  }

  def urlEncode(toEncode: String) = {
    val in = Utf8.encode(ensureUppercasedEncodings(toEncode))
    val out = CharBuffer.allocate((in.remaining() * 3).ceil.toInt)
    while (in.hasRemaining) {
      val b = in.get() & 0xFF
      if (toSkip.contains(b)) {
        out.put(b.toInt.toChar)
      } else {
        out.put('%')
        out.put(HexUpperCaseChars((b >> 4) & 0xF))
        out.put(HexUpperCaseChars(b & 0xF))
      }
    }
    out.flip()
    out.toString
  }

  def urlDecode(toDecode: String) = {
    val in = CharBuffer.wrap(toDecode)
    val out = ByteBuffer.allocate(in.remaining())
    while (in.hasRemaining) {
      val mark = in.position()
      val c = in.get()
      if (c == '%') {
        if (in.remaining() >= 2) {
          val x = Character.digit(in.get(), 0x10)
          val y = Character.digit(in.get(), 0x10)
          if (x != -1 && y != -1) {
            val oo = (x << 4) + y
            out.put(oo.toByte)
          } else {
            in.position(mark)
          }
        } else {
          in.position(in.position() - 1)
        }
      } else if (c == '+') {
        out.put(' '.toByte)
      } else {
        out.put(c.toByte)
      }
    }
    out.flip()
    Utf8.decode(out).toString
  }
}

object UrlCodingUtils extends UrlCodingUtils