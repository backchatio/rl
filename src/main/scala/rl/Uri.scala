package rl

import util.parsing.combinator._
import rl.UrlCodingUtils._
import rl.UriPath._
import java.lang.{ UnsupportedOperationException, Boolean }
import java.net.{ URISyntaxException, IDN, URI }

trait UriNode {
  def uriPart: String
}

trait UriOperations {
  def +(other: Uri): Uri
  def normalize: Uri
  def /(other: Uri): Uri
}

trait MonadicUri {

}

trait Uri {
  def scheme: UriScheme
  def authority: Option[Authority]
  def segments: UriPath
  def rawQuery: QueryString
  def fragment: UriFragment

  def originalUri: String
  def isAbsolute: Boolean
  def isRelative: Boolean

  def asciiString = {
    scheme.uriPart + authority.map(_.uriPart).getOrElse("") + segments.uriPart + rawQuery.uriPart + fragment.uriPart
  }
}

case class AbsoluteUri(scheme: Scheme, authority: Option[Authority], segments: UriPath, rawQuery: QueryString, fragment: UriFragment, originalUri: String = "") extends Uri {
  val isAbsolute: Boolean = true
  val isRelative: Boolean = false

}

case class RelativeUri(authority: Option[Authority], segments: UriPath, rawQuery: QueryString, fragment: UriFragment, originalUri: String = "") extends Uri {
  val scheme = NoScheme

  val isAbsolute: Boolean = false
  val isRelative: Boolean = true
}

case class FailedUri(msg: String, originalUri: String = "") extends Uri {

  private def noop = {
    val u = originalUri.toOption getOrElse "not set"
    throw new UnsupportedOperationException("Parsing the uri '%s' failed with:\n%s" format (u, msg))
  }

  def fragment = noop

  def rawQuery = noop

  def segments = noop

  def authority = noop

  def scheme = noop

  val isRelative: Boolean = false

  val isAbsolute: Boolean = false
}

object Uri {

  /*
   * The regex to split a URI up into its parts for further processing
   * Source: http://tools.ietf.org/html/rfc3986#appendix-B
   */
  val UriParts = """^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?""".r


  def apply(uriString: String) = {
    try {
      val parsed = URI.create(uriString)
      val pth = parsed.getRawPath.toOption match {
        case None                           ⇒ EmptyPath
        case Some(pt) if pt.startsWith("/") ⇒ AbsolutePath(pt.split("/"))
        case Some(pt)                       ⇒ RelativePath(pt.split("/"))
      }

      if (parsed.isAbsolute) {
        AbsoluteUri(
          Scheme(parsed.getScheme),
          Some(Authority(parsed.getRawAuthority)),
          pth,
          QueryString(parsed.getRawQuery),
          UriFragment(parsed.getRawFragment))
      } else {
        RelativeUri(
          Some(Authority(parsed.getRawAuthority)),
          pth,
          QueryString(parsed.getRawQuery),
          UriFragment(parsed.getRawFragment))
      }
    } catch {
      case e: URISyntaxException   ⇒ FailedUri(e.getMessage, uriString)
      case e: NullPointerException ⇒ FailedUri("A uri string can't be null", uriString)
      case e                       ⇒ FailedUri(e.getMessage, uriString)
    }
  }

}
