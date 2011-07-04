package rl
package tests

import org.specs2.Specification
import rl.UrlCodingUtils._

class UrlCodingSpec extends Specification { def is=

  "Encoding a URI should" ^
    "not change any of the allowed chars" ! {
      val encoded = UrlEncoded("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890!$&'()*+,;=:/?#[]@-._~")
      encoded must_== "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890!$&'()*+,;=:/?#[]@-._~"
    } ^
    "uppercase encodings already in a string" ! {
      ensureUppercasedEncodings("hello%3fworld") must_== "hello%3Fworld"
    } ^
    "percent encode spaces" ! {
      UrlEncoded("hello world") must_== "hello%20world"
    } ^
    "encode a letter with an accent as 2 values" ! {
      UrlEncoded("é") must_== "%C3%A9"
    } ^ p^
  "Decoding a URI should" ^
    "not change any of the allowed chars" ! {
      val decoded = UrlDecoded("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890!$&'()*+,;=:/?#[]@-._~")
      decoded must_== "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890!$&'()* ,;=:/?#[]@-._~"
    } ^
    "decode a pct encoded string" ! {
      UrlDecoded("hello%20world") must_== "hello world"
    } ^
    "decode value consisting of 2 values to 1 char" ! {
      UrlDecoded("%C3%A9") must_== "é"
    } ^ end

}