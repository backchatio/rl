package rl

import java.util.Date
import java.net.{ URI, URLEncoder }

object Benchmark extends App {

  val toEncode = "I'm a very long $tring with twitter://casualjim in, it"

  val url = "http://www.example.org/path/to/somewhere/"

  // warm-up
  (1 to 500) foreach { _ ⇒
    URLEncoder.encode(toEncode, "UTF-8")
    new URI(url)
  }

  (1 to 500) foreach { _ ⇒
    UrlCodingUtils.urlEncode(toEncode)
    Uri(url)
  }

  val start = new Date

  (1 to 1000000) foreach { _ ⇒
    URLEncoder.encode(toEncode, "UTF-8")
  }
  val jnetEnd = new Date

  (1 to 1000000) foreach { _ ⇒
    UrlCodingUtils.urlEncode(toEncode)
  }

  val end = new Date

  (1 to 100000) foreach { _ ⇒
    new URI(url).toASCIIString
  }

  val jurl = new Date

  (1 to 100000) foreach { _ ⇒
    Uri(url).asciiString
  }

  val uriend = new Date

  val javaTook = jnetEnd.getTime - start.getTime
  val rlTook = end.getTime - jnetEnd.getTime
  val juriTook = jurl.getTime - end.getTime
  val rluriTook = uriend.getTime - jurl.getTime
  println("Started: %s" format start)
  println("To encode 1000000 uri's")
  println("Java took: %s millis" format javaTook)
  println("RL took: %s millis" format rlTook)
  println("")
  println("To parse 100000 uri's")
  println("java took: %s millis" format juriTook)
  println("rl took: %s millis" format rluriTook)
}