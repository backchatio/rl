package rl

import util.parsing.combinator._
import java.net.IDN
import collection.GenSeq

sealed trait URINode



object UserInfo {
  def apply(userInfo: String): Option[UserInfo] = {
    userInfo.toOption map  { uif =>
      val Array(user, secret) = if (uif.indexOf(":") > -1) (uif.toString split ':') else Array(uif, "")
      UserInfo(user, secret)
    }
  }
}
case class UserInfo(user: String, secret: String) extends URINode {
  override def toString = (user /: secret.toOption) { _ + ":" + _ }
}

object Authority {
  def apply(authority: String): Authority = {
    val `has @` = (authority indexOf '@') > -1
    val Array(uif, auu) = if (`has @`) authority split '@' else Array("", authority)
    val au = if (auu.startsWith("@")) auu substring 1 else auu

    val uinf = UserInfo(uif)
    val `has :` = au.indexOf(':') > -1
    if (`has :`) {
      val Array(h, port) = au split ':'
      new Authority(uinf, h, Some(port.toInt))
    } else new Authority(uinf, au, None)
  }
}

case class Authority(userInfo: Option[UserInfo], host: String, port: Option[Int]) extends URINode {

  override def toString = {
//    ((userInfo map { _.toString }) :\ ((host /: port) { _ + ":" + _ })) { _ + "@" + _ }
    (userInfo map { _.toString + "@" } getOrElse "") + host + (port map { ":" + _ } getOrElse "") //expresses intent better
  }
}
//case class Uri(
//    scheme: String,
//    authority: Authority,
//    path: String,
//    rawQuery: String, //actually a sorted map
//    fragment: String)

object Uri {

  private val DEFAULT_PORTS = Map(
    "http" -> 80,
    "https" -> 443,
    "ftp" -> 21,
    "tftp" -> 69,
    "sftp" -> 22,
    "ssh" -> 22,
    "svn+ssh" -> 22,
    "git" -> 22,
    "git+ssh" -> 22,
    "telnet" -> 23,
    "nntp" -> 119,
    "gopher" -> 70,
    "wais" -> 210,
    "ldap" -> 389,
    "prospero" -> 1525,
    "smtp" -> 25,
    "imap" -> 143,
    "imaps" -> 993,
    "pop3" -> 110,
    "pop3s" -> 995,
    "redis" -> 6379,
    "mongo" -> 27017
  )

  /*
   * The regex to split a URI up into its parts for further processing
   * Source: http://tools.ietf.org/html/rfc3986#appendix-B
   */
  val UriParts = """^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?""".r

  private val subDelimChars = """[!$&'()*+,;=]""".r
  private val genDelimChars = """[:/?#\[\]@]""".r
  private val hexDigits = """[0123456789abcdefABCDEF]""".r

  val IPv6AddressRegex =
    ("""(?iu)^(((?=(?>.*?::)(?!.*::)))(::)?([0-9A-F]{1,4}::?){0,5}|([0-9A-F]{1,4}:){6})(\2([0-9A-F]{1,4}(::?|$)){0,2}""" +
        """|((25[0-5]|(2[0-4]|1[0-9]|[1-9])?[0-9])(\.|$)){4}|[0-9A-F]{1,4}:[0-9A-F]{1,4})(?<![^:]:)(?<!\.)\z""").r

  trait IPv6AddressParser extends RegexParsers {
    def hexDigit = hexDigits
    def decOctet = """25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d""".r
    def dottedDecOctet = decOctet <~ "."

    private def IPv4Address = dottedDecOctet ~ dottedDecOctet ~ dottedDecOctet ~ decOctet ^^ {
      case a ~ b ~ c ~ d => a + "." + b + "." + c + "." + d
    }


    private def h16_2 = repN(2, hexDigit)
    private def h16_3 = repN(3, hexDigit)
    private def h16_4 = repN(4, hexDigit)
    private def h16_multi = (h16_4 | h16_3 | h16_2) ^^ { _ mkString "" }
    private def h16 = h16_multi | hexDigit

    private def h16Colon = h16 ~ ":" ^^ { case a ~ b => a + b }
    private def h16Colon_2 = h16Colon ~ h16Colon ^^ { case a ~ b => a + b }
    private def h16Colon_3 = repN(3, h16Colon) ^^ { _ mkString "" }
    private def h16Colon_4 = repN(4, h16Colon) ^^ { _ mkString "" }
    private def h16Colon_5 = repN(5, h16Colon) ^^ { _ mkString "" }
    private def h16Colon_6 = repN(6, h16Colon) ^^ { _ mkString "" }
    private def h16Wrap(parser: Parser[String]): Parser[String] = parser ~ h16 ^^ { case a ~ b => a + b }
    private def h16ColonN(max: Int) = max match {
      case 6 => h16Wrap(h16Colon_6) | h16Wrap(h16Colon_5) | h16Wrap(h16Colon_4) | h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
      case 5 => h16Wrap(h16Colon_5) | h16Wrap(h16Colon_4) | h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
      case 4 => h16Wrap(h16Colon_4) | h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
      case 3 => h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
      case 2 => h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
      case 1 => h16Wrap(h16Colon)
    }
    private def h16Colonh16N(max: Int) = h16ColonN(max) | h16
    private def nH16Colon(n: Int) = repN(n, h16Colon) ^^ { _ mkString "" }

    private def flatOpt(parser: => Parser[String]): Parser[String] = opt(parser) ^^ { _ getOrElse  ""}

    private def ls32 = (h16Colon ~ h16 ^^ { case a ~ b => a + b } ) | IPv4Address

    private def ip6_1 = nH16Colon(6) ~ ls32 ^^ { case a ~ b => a + b }
    private def ip6_2 = "::" ~ nH16Colon(5) ~ ls32 ^^ { case a ~ b ~ c => a + b + c }
    private def ip6_3 = flatOpt(h16) ~ "::" ~ nH16Colon(4) ~ ls32 ^^ { case a ~ b ~ c ~ d => a + b + c + d }
    private def ip6_4 = flatOpt(h16Colonh16N(1)) ~ "::" ~ nH16Colon(3) ~ ls32 ^^ { case a ~ b ~ c ~ d => a + b + c + d }
    private def ip6_5 = flatOpt(h16Colonh16N(2)) ~ "::" ~ nH16Colon(2) ~ ls32 ^^ { case a ~ b ~ c ~ d => a + b + c + d }
    private def ip6_6 = flatOpt(h16Colonh16N(3)) ~ "::" ~ nH16Colon(1) ~ ls32 ^^ { case a ~ b ~ c ~ d => a + b + c + d }
    private def ip6_7 = flatOpt(h16Colonh16N(4)) ~ "::" ~ ls32 ^^ { case a ~ b ~ c  => a + b + c }
    private def ip6_8 = flatOpt(h16Colonh16N(5)) ~ "::" ~ h16 ^^ { case a ~ b ~ c => a + b + c }
    private def ip6_9 = flatOpt(h16Colonh16N(6)) ~ "::" ^^ { case a ~ b => a + b }
    def IPv6Address = ip6_1 | ip6_2 | ip6_3 | ip6_4 | ip6_5 | ip6_6 | ip6_7 | ip6_8 | ip6_9

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

    def pctEncoded = "%" ~> hexDigit ~ hexDigit ^^ { case a ~ b => a + b }
    def pchar = unreserved | pctEncoded | subDelims | ":" | "@"

    def segmentNzNc = rep1(unreserved | pctEncoded | subDelims | "@") ^^ { p => PathSegmentNode(p mkString "") }
    def segmentNz = rep1(pchar) ^^ { p => PathSegmentNode(p mkString "") }
    def segment = rep(pchar) ^^ { p =>  PathSegmentNode(p mkString "") }

    def query = rep(pchar | "/" | "?") ^^ { q =>
      (q mkString "").toOption map (QueryStringNode(_))
    }
    def queryOpt = opt("?" ~> query) ^^ { _ flatMap { a => a } }

    def fragment = rep(pchar | "/" | "?") ^^ { l =>
      (l mkString "").toOption map (FragmentNode(_))
    }
    def fragmentOpt = opt("#" ~> fragment) ^^ { _ flatMap { a => a } }

    def pathSegments = rep("/" ~> segment) ^^ { _ filter (_.value.isNotBlank) }
    def pathRootless = segmentNz ~ pathSegments ^^ { case a ~ b => a :: b }
    def pathNoScheme = (segmentNzNc ~ pathSegments) ^^ { case a ~ b => RelativePathNode(a :: b) }
    def pathAbsolute = "/" ~> pathRootless ^^ { AbsolutePathNode(_) }
    def pathAbEmpty = opt("/" ~> pathRootless) ^^ { _ getOrElse Nil }
    def path = pathAbsolute | pathNoScheme

    def regName = rep(unreserved | pctEncoded | subDelims) ^^ { h => HostNode(h mkString "") }

    def ipv4Address = dottedDecOctet ~ dottedDecOctet ~ dottedDecOctet ~ decOctet ^^ {
      case a ~ b ~ c ~ d => {
        IPv4AddressNode(a, b, c, d)
      }
    }


    def ipv6Address = IPv6Address ^^ { IPv6AddressNode(_) }
    def ipv6Literal = "[" ~> ipv6Address <~ "]"

    def ipvFuturePt1 = "v" ~> rep1(hexDigit) <~ "." ^^ { _ mkString "" }
    def ipvFuturePt2 = rep1(unreserved | subDelims | ":") ^^ { _ mkString "" }
    def ipvFuture = ipvFuturePt1 ~ ipvFuturePt2 ^^ {
      case a ~ b => IPvFutureAddressNode("v" + a + "." + b)
    }
    def ipvFutureLiteral = "[" ~> ipvFuture <~ "]"
    
    def ipLiteral = ipv6Literal | ipvFutureLiteral

    def port = ":" ~> (rep(digit) ^^ { l => PortNode(l.mkString.toInt) })
    def host = ipLiteral | ipv4Address | regName
    def userInfo = (rep(unreserved | pctEncoded | subDelims | ":") ^^ { l => UserInfoNode(l mkString "") }) <~ "@"
    def authority = opt(userInfo) ~ host ~ opt(port) ^^ { case a ~ b ~ c => AuthorityNode(a, b, c) }

    def scheme = alpha ~ (rep(alpha | digit | "+" | "-" | ".") ^^ { _ mkString "" }) <~ ":" ^^ { case a ~ b => SchemeNode(a + b) }

    def pathWithAuthority = "//" ~> authority ~ pathAbEmpty ^^ { case a ~ b => PathWithAuthorityNode(a, b) }
    def hierarchicalPart = opt(pathWithAuthority | pathAbsolute | pathNoScheme)

    def relativeRef = hierarchicalPart ~ queryOpt ~ fragmentOpt ^^ { case a ~ b ~ c => RelativeUriNode(a, b, c) }
    def absoluteUri = scheme ~ hierarchicalPart ~ queryOpt ~ fragmentOpt ^^ { case a ~ b ~ c ~ d => AbsoluteUriNode(a, b, c, d) }

    def uri = absoluteUri | relativeRef

    def apply(toParse: String) = {
      parseAll(uri, toParse) match {
        case Success(node, _) => node
        case Failure(msg, _) => FailedUri(msg)
      }
    }
  }

  private[rl] object UriParser extends UriParser

  private[rl] sealed trait UriPartNode
  private[rl] case class FragmentNode(value: String) extends UriPartNode
  private[rl] case class QueryStringNode(value: String) extends UriPartNode

  private[rl] sealed trait HostNodePart extends UriPartNode
  private[rl] sealed trait IPAddress extends HostNodePart
  private[rl] case class IPv4AddressNode(first: String, second: String, third: String, fourth: String) extends IPAddress {
    def address = {
      "%s.%s.%s.%s".format(first, second, third, fourth)
    }
  }
  private[rl] sealed trait IPLiteralNode extends IPAddress { def value: String }
  private[rl] case class IPvFutureAddressNode(value: String) extends IPLiteralNode
  private[rl] case class IPv6AddressNode(value: String) extends IPLiteralNode

  private[rl] case class UserInfoNode(value: String) extends UriPartNode
  private[rl] case class PortNode(value: Int) extends UriPartNode
  private[rl] case class HostNode(value: String) extends HostNodePart
  private[rl] case class AuthorityNode(userInfo: Option[UserInfoNode], host: HostNodePart, port: Option[PortNode]) extends UriPartNode

  private[rl] sealed trait PathPartNodePart extends UriPartNode
  private[rl] case class PathSegmentNode(value: String) extends PathPartNodePart
  private[rl] sealed trait PathNodePart extends UriPartNode
  private[rl] case class RelativePathNode(segments: GenSeq[PathSegmentNode]) extends PathNodePart
  private[rl] case class AbsolutePathNode(segments: GenSeq[PathSegmentNode]) extends PathNodePart
  private[rl] case class PathWithAuthorityNode(authority: AuthorityNode, path: GenSeq[PathSegmentNode]) extends PathNodePart

  private[rl] case class SchemeNode(value: String) extends UriPartNode

  private[rl] sealed trait UriNode
  private[rl] case class RelativeUriNode(path: Option[PathNodePart], query: Option[QueryStringNode], fragment: Option[FragmentNode]) extends UriNode
  private[rl] case class AbsoluteUriNode(scheme: SchemeNode, path: Option[PathNodePart], query: Option[QueryStringNode], fragment: Option[FragmentNode]) extends UriNode
  private[rl] case class FailedUri(msg: String) extends UriNode

  private def internationalize(parts: (String, String, String, String, String)) = {
    val (sch, auth, pth, qry, frag) = parts
    auth.toOption map { rawAuth =>
      val a = Authority(rawAuth)
      (sch, a.copy(host = IDN.toASCII(a.host)).toString, pth, qry, frag)
    } getOrElse {
      parts
    }
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

  private val wlpExpr = """^[A-Za-z]:\\""".r
  private val wuncpExpr = """^\\\\""".r
  private val windowsSeparator = "\\"
  private val unixSeparator = "/"

  def normalizeWindowsPath(path: String) = {
    if (wlpExpr.findFirstIn(path).isDefined) {
      "file:///" + windowsToUnixPath(path)
    } else if (wuncpExpr.findFirstIn(path).isDefined) {
      "file:" + windowsToUnixPath(path)
    } else windowsToUnixPath(path)
  }

  private def windowsToUnixPath(path: String) = path.replace(windowsSeparator, unixSeparator)


  def apply(uriString: String) {
    val pathNormalized = normalizeWindowsPath(uriString)
    val tokens = tokenize(pathNormalized)
    val i13l = internationalize(tokens)
    val toParse = if (tokens._2 != i13l._2) pathNormalized.replace(tokens._2, i13l._2) else pathNormalized
    /*
     * TODO: Maybe try to guess if the uri has already been percent encoded or contains invalid chars.
     * if not percent encoded and it contains invalid chars then percent encode it.
     */
    UriParser(toParse) match {
      case FailedUri(msg) => None
      case x => Some(x)
    }
  }
  
}
