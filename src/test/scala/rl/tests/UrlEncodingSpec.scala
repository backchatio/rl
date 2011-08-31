package rl
package tests

import org.specs2.Specification
import org.specs2.execute._
import rl.UrlCodingUtils._

class UrlEncodingSpec extends Specification {
  def is =
    "Section 2.1 Percent-Encoding" ^
      """A percent-encoded octet is encoded as a character
     triplet, consisting of the percent character "%" followed by the two
     hexadecimal digits representing that octet's numeric value""" ! {
        val space = " "
        urlEncode(space) must_== "%20"
      } ^
      "Section 2.4 describes _when_ to apply percent encoding" ! {
        Pending("Section 2.4 is not ready")
      } ^ p ^
      "Section 2.2 Reserved Characters" ^
      "it does not encode general delimiters (gen-delims)" ! {
        val generalDelimiters = ":/?#[]@"
        urlEncode(generalDelimiters) must_== generalDelimiters
      } ^
      "it does not encode subset delimiters (sub-delims)" ! {
        val subDelimiters = "!$&'()*+,;="
        urlEncode(subDelimiters) must_== subDelimiters
      } ^ p ^
      "Section 2.3 Unreserved Characters" ^
      "it does not encode unreserved characters" ! {
        val ALPHA = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        val DIGIT = "01234567890"
        val unreservedCharacters = ALPHA + DIGIT + "-._~"
        val result = urlEncode(unreservedCharacters)
        result must_== unreservedCharacters
      } ^ end
}