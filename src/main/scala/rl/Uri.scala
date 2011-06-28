package rl

import util.parsing.combinator._

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

  private object PartsValidator {
    val alpha = "a-zA-z"
    val digit = "0-9"
    val genDelims = """:/?#\[\]@"""
    val subDelims = """\!\$\&\'\(\)\*\+\,\;\="""
    val reserved = genDelims + subDelims
    val unreserved = alpha + digit + "-._~"
    val pchar = unreserved + subDelims + ":@"
    val scheme = alpha + digit + "-+."
    val authority = pchar
    val path = pchar + "/"
    val query = pchar + "/?"
    val fragment = pchar + "/?"

    def ri(expr: String) = "[%s]".format(expr).r
    def re(expr: String) = "[^%s]".format(expr).r
  }

  /*
   * The regex to split a URI up into its parts for further processing
   * Source: http://tools.ietf.org/html/rfc3986#appendix-B
   */
  private val UriParts = """^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?""".r


  /**
   * Returns a [[rl.Uri]] object based on the parsed string. This method is very lenient and just tokenizes the string
   * to the uri parts and creates a [[rl.Uri]] object with those parts. You probably want to call validate on that
   * returned object afterwards
   *
   * @param uriString the [[scala.String]] to tokenize
   *
   * @return the parsed denormalized [[rl.Uri]]
   */

  private[rl] def tokenize(uriString: String) = {
    val UriParts(_, sch, _, auth, pth, _, qry, _, frag) = uriString
    Uri(sch, Authority(auth), pth, qry, frag)
  }

  private val subDelimChars = """[!$&'()*+,;=]""".r
  private val genDelimChars = """[:/?#\[\]@]""".r
  private val hexDigits = """[0123456789abcdefABCDEF]""".r

  trait UriParser extends RegexParsers {

    def subDelims = subDelimChars
    def genDelims = genDelimChars
    def reserved = genDelims | subDelims
    def alpha = """[a-zA-Z]""".r
    def digit = """\d""".r

    def unreserved = alpha | digit | "-" | "." | "_" | "~"
    def hexDigit = hexDigits
    def pctEncoded = "%" ~ hexDigit ~ hexDigit ^^ { case a ~ b => a + b }
    def pchar = unreserved | pctEncoded | subDelims | ":" | "@"

    def segmentNzNc = rep1(unreserved | pctEncoded | subDelims | "@") ^^ { _ mkString "" }
    def segmentNz = rep1(pchar) ^^ { _ mkString "" }
    def segment = rep(pchar) ^^ { _ mkString "" }

    def query = rep(pchar | "/" | "?") ^^ { l => QueryStringNode(l mkString "") }
    def fragment = rep(pchar | "/" | "?") ^^ { l => FragmentNode(l mkString "") }


    def pathSegments = rep("/" ~ segment) ^^ { _ mkString "" }
    def pathRootless = segmentNz ~ pathSegments ^^ { case a ~ b => a + b }
    def pathNoScheme = segmentNzNc ~ pathSegments ^^ { case a ~ b => a + b }
    def optPath = opt(pathRootless) ^^ { _ getOrElse "" }
    def pathAbsolute = "/" ~ optPath ^^ { case a ~ b => a + b }
    def pathAbEmpty = rep("/" ~ segment) ^^ { _ mkString "" }
    def path = (pathAbEmpty | pathAbsolute | pathNoScheme | pathRootless) ^^ { PathNode(_) }

    def regName = rep(unreserved | pctEncoded | subDelims) ^^ { _ mkString "" }

    def decOctet = """25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d""".r

    def ipv4Address = decOctet ~ "." ~ decOctet ~ "." ~ decOctet ~ "." ~ decOctet ^^ { case a ~ b => a + b }
    def ipv6Address = IPv6Address
    def ipvFuturePt1 = rep1(hexDigit) ^^ { _ mkString "" }
    def ipvFuturePt2 = rep1(unreserved | subDelims | ":") ^^ { _ mkString "" }
    def ipvFuture = "v" ~ ipvFuturePt1 ~ "." ~ ipvFuturePt2

    def ipLiteral = "[" ~ (ipv6Address | ipvFuture) ~ "]" ^^ { case a ~ b => a + b }

    def port = rep(digit) ^^ { _ mkString "" }
    def optPort = opt(":" ~> port) ^^ { _ map (":" + _) getOrElse "" }
    def host = ipLiteral | ipv4Address | regName
    def userinfo = rep(unreserved | pctEncoded | subDelims | ":") ^^ { _ mkString "" }
    def optUserInfo = opt(userinfo <~ "@") ^^ { a => a map (_ + "@") getOrElse "" }
    def authority = optUserInfo ~ host ~ optPort ^^ { case a ~ b => a + b }

    def scheme = alpha ~ (rep(alpha | digit | "+" | "-" | ".") ^^ { _ mkString "" }) ^^ { case a ~ b => a + b }
    

  }
  sealed trait UriAstNode
  case class FragmentNode(value: String) extends UriAstNode
  case class QueryStringNode(value: String) extends UriAstNode
  case class PathNode(value: String) extends UriAstNode
  case class UserNode(value: String) extends UriAstNode
  case class PasswordNode(value: String) extends UriAstNode
  case class UserInfoNode(user: UserNode, password: PasswordNode) extends UriAstNode
  case class PortNode(value: Int) extends UriAstNode
  case class AuthorityNode(value: String, userInfo: Option[UserInfoNode], port: Option[PortNode]) extends UriAstNode
  case class SchemeNode(value: String) extends UriAstNode
  case class ParsedUri(
               scheme: Option[SchemeNode],
               authority: Option[AuthorityNode],
               path: Option[PathNode],
               query: Option[QueryStringNode],
               fragment: Option[FragmentNode]) extends UriAstNode
  
}
