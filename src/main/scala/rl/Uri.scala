package rl

import util.parsing.combinator._
import java.net.IDN
import rl.UrlCodingUtils._
import rl.UriPath._
import java.lang.{ UnsupportedOperationException, Boolean }

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

  def isRelative = false

  def isAbsolute = false
}

object Uri {

  /*
   * The regex to split a URI up into its parts for further processing
   * Source: http://tools.ietf.org/html/rfc3986#appendix-B
   */
  val UriParts = """^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?""".r

  private val subDelimChars = """[!$&'()*+,;=]""".r
  private val genDelimChars = """[:/?#\[\]@]""".r
  private val hexDigits = """[0123456789abcdefABCDEF]""".r

  trait IPv6AddressParser extends RegexParsers {
    def hexDigit = hexDigits
    def decOctet = """25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d""".r
    def dottedDecOctet = decOctet <~ "."

    private def IPv4Address = dottedDecOctet ~ dottedDecOctet ~ dottedDecOctet ~ decOctet ^^ {
      case a ~ b ~ c ~ d ⇒ a + "." + b + "." + c + "." + d
    }

    private def h16_2 = repN(2, hexDigit)
    private def h16_3 = repN(3, hexDigit)
    private def h16_4 = repN(4, hexDigit)
    private def h16_multi = (h16_4 | h16_3 | h16_2) ^^ { _ mkString "" }
    private def h16 = h16_multi | hexDigit

    private def h16Colon = h16 ~ ":" ^^ { case a ~ b ⇒ a + b }
    private def h16Colon_2 = h16Colon ~ h16Colon ^^ { case a ~ b ⇒ a + b }
    private def h16Colon_3 = repN(3, h16Colon) ^^ { _ mkString "" }
    private def h16Colon_4 = repN(4, h16Colon) ^^ { _ mkString "" }
    private def h16Colon_5 = repN(5, h16Colon) ^^ { _ mkString "" }
    private def h16Colon_6 = repN(6, h16Colon) ^^ { _ mkString "" }
    private def h16Wrap(parser: Parser[String]): Parser[String] = parser ~ h16 ^^ { case a ~ b ⇒ a + b }
    private def h16ColonN(max: Int) = max match {
      case 6 ⇒ h16Wrap(h16Colon_6) | h16Wrap(h16Colon_5) | h16Wrap(h16Colon_4) | h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
      case 5 ⇒ h16Wrap(h16Colon_5) | h16Wrap(h16Colon_4) | h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
      case 4 ⇒ h16Wrap(h16Colon_4) | h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
      case 3 ⇒ h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
      case 2 ⇒ h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
      case 1 ⇒ h16Wrap(h16Colon)
    }
    private def h16Colonh16N(max: Int) = h16ColonN(max) | h16
    private def nH16Colon(n: Int) = repN(n, h16Colon) ^^ { _ mkString "" }

    private def flatOpt(parser: ⇒ Parser[String]): Parser[String] = opt(parser) ^^ { _ getOrElse "" }

    private def ls32 = (h16Colon ~ h16 ^^ { case a ~ b ⇒ a + b }) | IPv4Address

    private def ip6_1 = nH16Colon(6) ~ ls32 ^^ { case a ~ b ⇒ a + b }
    private def ip6_2 = "::" ~ nH16Colon(5) ~ ls32 ^^ { case a ~ b ~ c ⇒ a + b + c }
    private def ip6_3 = flatOpt(h16) ~ "::" ~ nH16Colon(4) ~ ls32 ^^ { case a ~ b ~ c ~ d ⇒ a + b + c + d }
    private def ip6_4 = flatOpt(h16Colonh16N(1)) ~ "::" ~ nH16Colon(3) ~ ls32 ^^ { case a ~ b ~ c ~ d ⇒ a + b + c + d }
    private def ip6_5 = flatOpt(h16Colonh16N(2)) ~ "::" ~ nH16Colon(2) ~ ls32 ^^ { case a ~ b ~ c ~ d ⇒ a + b + c + d }
    private def ip6_6 = flatOpt(h16Colonh16N(3)) ~ "::" ~ nH16Colon(1) ~ ls32 ^^ { case a ~ b ~ c ~ d ⇒ a + b + c + d }
    private def ip6_7 = flatOpt(h16Colonh16N(4)) ~ "::" ~ ls32 ^^ { case a ~ b ~ c ⇒ a + b + c }
    private def ip6_8 = flatOpt(h16Colonh16N(5)) ~ "::" ~ h16 ^^ { case a ~ b ~ c ⇒ a + b + c }
    private def ip6_9 = flatOpt(h16Colonh16N(6)) ~ "::" ^^ { case a ~ b ⇒ a + b }
    def IP6Address = ip6_1 | ip6_2 | ip6_3 | ip6_4 | ip6_5 | ip6_6 | ip6_7 | ip6_8 | ip6_9

    override def skipWhitespace = false
  }

  /**
   * Implements a parser for the ABNF found in appendix A of RFC-3986
   * Source: http://tools.ietf.org/html/rfc3986#appendix-A
   */
  private[rl] trait UriParser extends IPv6AddressParser {

    def subDelims = subDelimChars
    def genDelims = genDelimChars
    def reserved = genDelims | subDelims
    def alpha = """[a-zA-Z]""".r
    def digit = """\d""".r

    def unreserved = alpha | digit | "-" | "." | "_" | "~"

    def pctEncoded = "%" ~ hexDigit ~ hexDigit ^^ { case a ~ b ~ c ⇒ a + b + c }
    def pchar = unreserved | pctEncoded | subDelims | ":" | "@"

    def segmentNzNc = rep1(unreserved | pctEncoded | subDelims | "@") ^^ { _ mkString "" }
    def segmentNz = rep1(pchar) ^^ { _ mkString "" }
    def segment = rep(pchar) ^^ { _ mkString "" }

    def query = rep(pchar | "/" | "?") ^^ { q ⇒
      ((q mkString "").toOption map { v ⇒
        (v.indexOf('&') > -1, v.indexOf('=') > -1) match {
          case (true, true) | (false, true) ⇒ MapQueryString(v)
          case (true, false) ⇒ StringSeqQueryString(v)
          case (false, false) ⇒ StringQueryString(v)
        }
      }) getOrElse EmptyQueryString
    }
    def queryOpt = opt("?" ~> query) ^^ { _ getOrElse EmptyQueryString }

    def fragment = rep(pchar | "/" | "?") ^^ { l ⇒
      (l mkString "").toOption map (StringFragment(_)) getOrElse EmptyFragment
    }
    def fragmentOpt = opt("#" ~> fragment) ^^ { _ getOrElse EmptyFragment }

    def pathSegments = rep("/" ~> segment) ^^ { _ filter (_.isNotBlank) }
    def pathRootless = segmentNz ~ pathSegments ^^ { case a ~ b ⇒ a :: b }
    def pathNoScheme = segmentNzNc ~ pathSegments ^^ { case a ~ b ⇒ RelativePath(a :: b) }
    def pathAbsolute = "/" ~> pathRootless ^^ { AbsolutePath(_) }
    def pathAbEmpty = opt("/") ^^ { _ map { _ ⇒ EmptyAbsolutePath } getOrElse EmptyRelativePath }
    def path = pathAbsolute | pathNoScheme | pathAbEmpty

    def regName = rep(unreserved | pctEncoded | subDelims) ^^ { h ⇒ HostName(h mkString "") }

    def ipv4Address = dottedDecOctet ~ dottedDecOctet ~ dottedDecOctet ~ decOctet ^^ {
      case a ~ b ~ c ~ d ⇒ IPv4Address(a + "." + b + "." + c + "." + d)
    }

    def ipv6Address = IP6Address ^^ { IPv6Address(_) }
    def ipv6Literal = "[" ~> ipv6Address <~ "]"

    def ipvFuturePt1 = "v" ~> rep1(hexDigit) <~ "." ^^ { _ mkString "" }
    def ipvFuturePt2 = rep1(unreserved | subDelims | ":") ^^ { _ mkString "" }
    def ipvFuture = ipvFuturePt1 ~ ipvFuturePt2 ^^ {
      case a ~ b ⇒ IPvFutureAddress("v" + a + "." + b)
    }
    def ipvFutureLiteral = "[" ~> ipvFuture <~ "]"

    def ipLiteral = ipv6Literal | ipvFutureLiteral

    def port = ":" ~> (rep(digit) ^^ { _.mkString.toInt })
    def host = ipLiteral | ipv4Address | regName
    def userInfo = (rep(unreserved | pctEncoded | subDelims | ":") ^^ { u ⇒ UserInfo(u mkString "") }) <~ "@"
    def authority = (opt(userInfo) ^^ { _ flatMap { a ⇒ a } }) ~ host ~ opt(port) ^^ { case a ~ b ~ c ⇒ Authority(a, b, c) }

    def scheme = alpha ~ (rep(alpha | digit | "+" | "-" | ".") ^^ { _ mkString "" }) <~ ":" ^^ { case a ~ b ⇒ Scheme(a + b) }

    def pathWithAuthority = "//" ~> authority ~ path ^^ { case a ~ b ⇒ (Some(a), b) }
    def absolutePathWithoutAuthority = pathAbsolute ^^ { (None.asInstanceOf[Option[Authority]], _) }
    def relativePathWithoutAuthority = pathNoScheme ^^ { (None.asInstanceOf[Option[Authority]], _) }

    def hierarchicalPart = pathWithAuthority | absolutePathWithoutAuthority | relativePathWithoutAuthority

    def relativeRef = hierarchicalPart ~ queryOpt ~ fragmentOpt ^^ { case a ~ b ~ c ⇒ RelativeUri(a._1, a._2, b, c) }
    def absoluteUri = scheme ~ hierarchicalPart ~ queryOpt ~ fragmentOpt ^^ { case a ~ b ~ c ~ d ⇒ AbsoluteUri(a, b._1, b._2, c, d) }

    def uri = absoluteUri | relativeRef

    def apply(toParse: String, originalUri: String): Uri = {
      parseAll(uri, toParse) match {
        case Success(node, _) ⇒ node match {
          case m: AbsoluteUri ⇒ m.copy(originalUri = originalUri)
          case m: RelativeUri ⇒ m.copy(originalUri = originalUri)
        }
        case Failure(msg, _) ⇒ FailedUri(msg, originalUri)
      }
    }
  }

  private[rl] object UriParser extends UriParser

  private def internationalize(parts: (String, String, String, String, String)) = {
    val (sch, auth, pth, qry, frag) = parts
    auth.toOption map { rawAuth ⇒
      val (_, a, _) = Authority(rawAuth)
      val h = IDN.toASCII(a)
      (sch, ensureUrlEncoding(h), pth, qry, frag)
    } getOrElse { parts }
  }

  /**
   * Returns a [[rl.Uri]] object based on the parsed string. This method is very lenient and just tokenizes the string
   * to the uri parts and creates a [[rl.Uri]] object with those parts. You probably want to call validate on that
   * returned object afterwards
   *
   * @param uriString the [[scala.String]] to tokenize
   *
   * @return the parsed unnormalized [[rl.Uri]]
   */
  private[rl] def tokenize(uriString: String) = {
    val UriParts(_, sch, _, auth, pth, _, qry, _, frag) = uriString
    (sch, auth, pth, qry, frag)
  }

  private def encodePaths(parts: (String, String, String, String, String)) = {
    val pth = parts._3
    parts.copy(_3 = ensureUrlEncoding(pth))
  }

  def apply(uriString: String) = {
    val unixifiedPath = windowsToUnixPath(uriString)
    val tokens = tokenize(unixifiedPath)
    val i13l = internationalize(tokens)
    val withI13lHost = if (tokens._2 != i13l._2) unixifiedPath.replace(tokens._2, i13l._2) else unixifiedPath
    val encodedPath = encodePaths(i13l)
    val toParse = if (tokens._3 != encodedPath._3) withI13lHost.replace(tokens._3, encodedPath._3) else withI13lHost
    UriParser(toParse, uriString)
  }

}
