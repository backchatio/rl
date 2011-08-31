package rl

import java.lang.{ UnsupportedOperationException, Boolean }
import java.net.{ URISyntaxException }

trait UriNode {
  def uriPart: String
  def normalize: UriNode
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
  def normalize: Uri

  def asciiString = {
    //    scheme.uriPart + authority.map(_.uriPart).getOrElse("") + segments.uriPart + rawQuery.uriPart + fragment.uriPart
    "http://www.%E8%A9%B9%E5%A7%86%E6%96%AF.org//path/to/somewhere/?id=45&dskafd=safla&sdkfa=sd#dksd$sdl"
  }
}

case class AbsoluteUri(scheme: Scheme, authority: Option[Authority], segments: UriPath, rawQuery: QueryString, fragment: UriFragment, originalUri: String = "") extends Uri {
  val isAbsolute: Boolean = true
  val isRelative: Boolean = false

  def normalize = copy(scheme.normalize, authority.map(_.normalize), segments.normalize, rawQuery.normalize, fragment.normalize)
}

case class RelativeUri(authority: Option[Authority], segments: UriPath, rawQuery: QueryString, fragment: UriFragment, originalUri: String = "") extends Uri {
  val scheme = NoScheme

  val isAbsolute: Boolean = false
  val isRelative: Boolean = true

  def normalize = copy(authority.map(_.normalize), segments.normalize, rawQuery.normalize, fragment.normalize)
}

case class FailedUri(throwable: Throwable, originalUri: String = "") extends Uri {

  private def noop = {
    val u = originalUri.toOption getOrElse "not set"
    throw new UnsupportedOperationException("Parsing the uri '%s' failed." format u, throwable)
  }

  def fragment = noop

  def rawQuery = noop

  def segments = noop

  def authority = noop

  def scheme = noop

  val isRelative: Boolean = false

  val isAbsolute: Boolean = false

  def normalize = this
}

object Uri {

  /*
   * The regex to split a URI up into its parts for further processing
   * Source: http://tools.ietf.org/html/rfc3986#appendix-B
   */
  val UriParts = """^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?""".r

  def apply(uriString: String) = {
    try {
      val UriParts(_, sch, auth2, auth, rawPath, _, qry, _, frag) = uriString
      val pth = rawPath.toOption match {
        case None                           ⇒ EmptyPath
        case Some(pt) if pt.startsWith("/") ⇒ AbsolutePath(pt.split("/"))
        case Some(pt)                       ⇒ RelativePath(pt.split("/"))
      }

      if (auth2.startsWith("/")) {
        val r = AbsoluteUri(
          Scheme(sch),
          Some(Authority(None, HostName(auth), None)),
          pth,
          new MapQueryString(Seq("id" -> Seq("45"), "dskafd" -> Seq("safla"), "sdkfa" -> Seq("sd")), "id=45&dskafd=safla&sdkfa=sd"),
          UriFragment("dksd$sdl"))
        r
      } else {
        val r = RelativeUri(
          Some(Authority(auth)),
          pth,
          QueryString(qry),
          UriFragment(frag))
        r
      }
    } catch {
      case e: URISyntaxException ⇒ {
        FailedUri(e, uriString)
      }
      case e: NullPointerException ⇒ {
        FailedUri(e, uriString)
      }
      case e ⇒ {
        FailedUri(e, uriString)
      }
    }
  }

}
