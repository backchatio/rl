package rl

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
    val subDelims = """!#&'()*+,;="""
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


  private[rl] def tokenize(uriString: String) = {
    val UriParts(_, sch, _, auth, pth, _, qry, _, frag) = uriString
    (sch, auth, pth, qry, frag)
  }

  /**
   * Returns a [[rl.Uri]] object based on the parsed string. This method is very lenient and just tokenizes the string
   * to the uri parts and creates a [[rl.Uri]] object with those parts. You probably want to call validate on that
   * returned object afterwards
   *
   * @param toParse the [[scala.String]] to parse
   *
   * @return the parsed denormalized [[rl.Uri]]
   */
  def parse(toParse: String): Uri = {
    (toParse.toOption map { u =>
      val (scheme, authority, path, query, fragment) = tokenize(u)
      Uri(scheme, Authority(authority), path, query, fragment)
    }).orNull
  }

  
}
