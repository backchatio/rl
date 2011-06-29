package rl

import util.parsing.combinator._
import java.net.IDN

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
case class Uri(
    scheme: String,
    authority: Authority,
    path: String,
    rawQuery: String, //actually a sorted map
    fragment: String)

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

  private val subDelimChars = """[!$&'()*+,;=]""".r
  private val genDelimChars = """[:/?#\[\]@]""".r
  private val hexDigits = """[0123456789abcdefABCDEF]""".r

  val IPv6AddressRegex =
    ("""(?iu)^(((?=(?>.*?::)(?!.*::)))(::)?([0-9A-F]{1,4}::?){0,5}|([0-9A-F]{1,4}:){6})(\2([0-9A-F]{1,4}(::?|$)){0,2}""" +
        """|((25[0-5]|(2[0-4]|1[0-9]|[1-9])?[0-9])(\.|$)){4}|[0-9A-F]{1,4}:[0-9A-F]{1,4})(?<![^:]:)(?<!\.)\z""").r


  /**
   * Implements a parser for the ABNF found in appendix A of RFC-3986
   * Source: http://tools.ietf.org/html/rfc3986#appendix-A
   */
  private[rl] trait UriParser extends RegexParsers {

    def subDelims = subDelimChars
    def genDelims = genDelimChars
    def reserved = genDelims | subDelims
    def alpha = """[a-zA-Z]""".r
    def digit = """\d""".r

    def unreserved = alpha | digit | "-" | "." | "_" | "~"
    def hexDigit = hexDigits
    def pctEncoded = "%" ~> hexDigit ~ hexDigit ^^ { case a ~ b => a + b }
    def pchar = unreserved | pctEncoded | subDelims | ":" | "@"

    def segmentNzNc = rep1(unreserved | pctEncoded | subDelims | "@") ^^ { _ mkString "" }
    def segmentNz = rep1(pchar) ^^ { _ mkString "" }
    def segment = rep(pchar) ^^ { _ mkString "" }

    def query = rep(pchar | "/" | "?") ^^ { q =>
      (q mkString "").toOption map (QueryStringNode(_))
    }
    def queryOpt = opt("?" ~> query) ^^ { _ flatMap { a => a } }

    def fragment = rep(pchar | "/" | "?") ^^ { l =>
      (l mkString "").toOption map (FragmentNode(_))
    }
    def fragmentOpt = opt("#" ~> fragment) ^^ { _ flatMap { a => a } }


    def pathSegments = rep("/" ~ segment) ^^ { _ mkString "" }
    def pathRootless = segmentNz ~ pathSegments ^^ { case a ~ b => a + b }
    def pathNoScheme = segmentNzNc ~ pathSegments ^^ { case a ~ b => a + b }
    def optPath = opt(pathRootless) ^^ { _ getOrElse "" }
    def pathAbsolute = "/" ~ optPath ^^ { case a ~ b => a + b }
    def pathAbEmpty = rep("/" ~ segment) ^^ { _ mkString "" }
    def path = pathAbEmpty | pathAbsolute | pathNoScheme | pathRootless

    def regName = rep(unreserved | pctEncoded | subDelims) ^^ { _ mkString "" }

    def decOctet = """25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d""".r
    def dottedDecOctet = decOctet <~ "."

    def ipv4Address = dottedDecOctet ~ dottedDecOctet ~ dottedDecOctet ~ decOctet ^^ {
      case a ~ b ~ c ~ d => {
        IPv4AddressNode(a, b, c, d)
      }
    }

    def h16_2 = repN(2, hexDigit)
    def h16_3 = repN(3, hexDigit)
    def h16_4 = repN(4, hexDigit)
    def h16_multi = (h16_4 | h16_3 | h16_2) ^^ { _ mkString "" }
    def h16 = h16_multi | hexDigit

    def h16Colon = h16 ~ ":" ^^ { case a ~ b => a + b }
    def h16Colon_2 = repN(2, h16Colon)
    def h16Colon_3 = repN(3, h16Colon)
    def h16Colon_4 = repN(4, h16Colon)
    def h16Colon_5 = repN(5, h16Colon)
    def h16Colon_6 = repN(6, h16Colon)
    def h16ColonN(max: Int) = max match {
      case 6 => ((h16Colon_6 | h16Colon_5 | h16Colon_4| h16Colon_3 | h16Colon_2) ^^ { _ mkString "" }) | h16Colon
      case 5 => ((h16Colon_5 | h16Colon_4| h16Colon_3 | h16Colon_2) ^^ { _ mkString "" }) | h16Colon
      case 4 => ((h16Colon_4 | h16Colon_3 | h16Colon_2) ^^ { _ mkString "" }) | h16Colon
      case 3 => ((h16Colon_3 | h16Colon_2) ^^ { _ mkString "" }) | h16Colon
      case 2 => (h16Colon_2 ^^ { _ mkString "" }) | h16Colon
      case 1 => h16Colon
    }
    def h16Colonh16N(max: Int) = h16ColonN(max) ~ h16 ^^ { case a ~ b => a + b }
    def nH16Colon(n: Int) = repN(n, h16Colon) ^^ { _ mkString "" }

    def flatOpt(parser: => Parser[String]): Parser[String] = opt(parser) ^^ { _ getOrElse  ""}

    def ls32 = (h16Colon ~ h16 ^^ { case a ~ b => a + b } ) | ipv4Address

    def ip6_1 = nH16Colon(6) ~ ls32 ^^ { case a ~ b => a + b }
    def ip6_2 = "::" ~ nH16Colon(6) ~ ls32 ^^ { case a ~ b ~ c => a + b + c }
    def ip6_3 = flatOpt(h16) ~ "::" ~ nH16Colon(4) ~ ls32 ^^ { case a ~ b ~ c ~ d => a + b + c + d }
    def ip6_4 = flatOpt(h16Colonh16N(1)) ~ "::" ~ nH16Colon(3) ~ ls32 ^^ { case a ~ b ~ c ~ d => a + b + c + d }
    def ip6_5 = flatOpt(h16Colonh16N(2)) ~ "::" ~ nH16Colon(2) ~ ls32 ^^ { case a ~ b ~ c ~ d => a + b + c + d }
    def ip6_6 = flatOpt(h16Colonh16N(3)) ~ "::" ~ nH16Colon(1) ~ ls32 ^^ { case a ~ b ~ c ~ d => a + b + c + d }
    def ip6_7 = flatOpt(h16Colonh16N(4)) ~ "::" ~ ls32 ^^ { case a ~ b ~ c  => a + b + c }
    def ip6_8 = flatOpt(h16Colonh16N(5)) ~ "::" ~ h16 ^^ { case a ~ b ~ c => a + b + c }
    def ip6_9 = flatOpt(h16Colonh16N(6)) ~ "::" ^^ { case a ~ b => a + b }
    def ipv6Address = (ip6_9 | ip6_8 | ip6_7 | ip6_6 | ip6_5 | ip6_4 | ip6_3 | ip6_2 | ip6_1) ^^ { IPv6AddressNode(_) }

    def ipvFuturePt1 = "v" ~> rep1(hexDigit) <~ "." ^^ { _ mkString "" }
    def ipvFuturePt2 = rep1(unreserved | subDelims | ":") ^^ { _ mkString "" }
    def ipvFuture = ipvFuturePt1 ~ ipvFuturePt2 ^^ {
      case a ~ b => IPvFutureAddressNode("v" + a + "." + b)
    }
    
    def ipLiteral = "[" ~> (ipv6Address | ipvFuture) <~ "]"

    def port = rep(digit) ^^ { l => PortNode(l.mkString.toInt) }
    def host = ipLiteral | ipv4Address | regName
    def userInfo = rep(unreserved | pctEncoded | subDelims | ":") ^^ { l => UserInfoNode(l mkString "") }
    def authority = opt(userInfo) ~ host ~ opt(port) ^^ { case a ~ b ~ c => AuthorityNode(a, HostNode(b), c) }

    def scheme = alpha ~ (rep(alpha | digit | "+" | "-" | ".") ^^ { _ mkString "" }) ^^ { case a ~ b => a + b }

    def pathWithAuthority = "//" ~ authority ~ pathAbEmpty ^^ { case a ~ b ~ c => a + b + c }
    def relativePart = opt(pathWithAuthority | pathAbsolute | pathNoScheme) ^^ { _ getOrElse "" }
    def relativeRef = relativePart ~ queryOpt ~ fragmentOpt ^^ { case a ~ b ~ c => a + b + c }

    def hierarchicalPart = opt(pathWithAuthority | pathAbsolute | pathRootless) ^^ { _ getOrElse "" }
    def absoluteUri = scheme ~ ":" ~ hierarchicalPart ~ queryOpt ^^ { case a ~ b ~ c ~ d => a + b + c + d }

    def uri = scheme ~ ":" ~ hierarchicalPart ~ queryOpt ~ fragmentOpt ^^ { case a ~ b ~ c ~ d ~ e => a + b + c + d + e }
    def uriReference = uri | relativeRef
  }

  private[rl] object UriParser extends UriParser

  private[rl] sealed trait UriPartNode
  private[rl] case class FragmentNode(value: String) extends UriPartNode
  private[rl] case class QueryStringNode(value: String) extends UriPartNode

  private[rl] sealed trait IPAddress extends UriPartNode
  private[rl] case class IPv4AddressNode(first: String, second: String, third: String, fourth: String) extends UriPartNode {
    def address = {
      "%s.%s.%s.%s".format(first, second, third, fourth)
    }
  }
  private[rl] sealed trait IPLiteralNode extends UriPartNode { def value: String }
  private[rl] case class IPvFutureAddressNode(value: String) extends IPLiteralNode
  private[rl] case class IPv6AddressNode(value: String) extends IPLiteralNode

  private[rl] case class UserInfoNode(value: String) extends UriPartNode
  private[rl] case class PortNode(value: Int) extends UriPartNode
  private[rl] case class HostNode(value: String) extends UriPartNode
  private[rl] case class AuthorityNode(userInfo: Option[UserInfoNode], host: HostNode, port: Option[PortNode]) extends UriPartNode
  

  private def internationalize(parts: (String, String, String, String, String)) = {
    val (sch, auth, pth, qry, frag) = parts
    (sch, IDN.toASCII(auth), pth, qry, frag)
  }

  def apply(uriString: String, includeFragment: Boolean = true) {
    
    // internationalize
    // validating parse
    // return object model
//    UriParser.parseAll(UriParser.uriReference, uriString)
  }
  
}
