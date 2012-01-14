package rl

import java.net._

object UserInfo {
  def apply(userInfo: String): Option[UserInfo] = {
    userInfo.blankOption map { uif ⇒
      val Array(user, secret) = if (uif.indexOf(":") > -1) (uif.toString split ':') else Array(uif, "")
      UserInfo(user, secret)
    }
  }
}
case class UserInfo(user: String, secret: String) extends UriNode {
  val uriPart = toString + "@"
  override def toString = (user /: secret.blankOption) { _ + ":" + _ }

  def normalize = this
  def apply() = toString
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
      Authority(uinf, HostName(h), Some(port.toInt))
    } else Authority(uinf, HostName(au), None)
  }

}

sealed trait UriHost extends UriNode {
  def value: String
  def normalize: UriHost with UriHostDomains

  def apply() = value
}

class EmptyHost extends UriHost {
  val uriPart = value

  val value = "/"

  def normalize = new EmptyHost with UriHostDomains {
    protected def parsed = ("/", "", "")
  }
}

case object DefaultEmptyHost extends EmptyHost
case class HostName(value: String) extends UriHost {

  val uriPart = UrlCodingUtils.ensureUrlEncoding(value)

  override def normalize = {
    // val va = if (value.startsWith("www.")) value.substring(4) else value
    val punycode = IDN.toASCII(value)
    new HostName(punycode) with UriHostDomains {
      protected val parsed = DomainParser(this.value)
    }
  }
}
case class IPv4Address(value: String) extends UriHost {
  val uriPart = value

  val bytes = value.split("\\.").map(_.toByte).toArray

  override def normalize = new IPv4Address(value) with UriHostDomains {
    protected val parsed = {
      try {
        DomainParser(InetAddress.getByAddress(bytes).getCanonicalHostName)
      } catch {
        case e: UnknownHostException ⇒ (value, "", "")
      }
    }
  }
}
case class IPv6Address(value: String) extends UriHost {
  val uriPart = "[" + value + "]"
  def normalize: UriHost with UriHostDomains = new IPv6Address(value) with UriHostDomains {
    protected val parsed = (value, "", "")
  }
}
case class IPvFutureAddress(value: String) extends UriHost {
  val uriPart = "[" + value + "]"
  def normalize: UriHost with UriHostDomains = new IPvFutureAddress(value) with UriHostDomains {
    protected val parsed = (value, "", "")
  }
}

case class Authority(userInfo: Option[UserInfo], host: UriHost, port: Option[Int]) extends UriNode {

  def normalize = copy(userInfo.map(_.normalize), host.normalize, port)

  val uriPart = "//" + toString + "/"
  override def toString = {
    (userInfo map { _.uriPart } getOrElse "") + host.uriPart + (port map { ":" + _ } getOrElse "") //expresses intent better
  }

  def apply() = toString
}

