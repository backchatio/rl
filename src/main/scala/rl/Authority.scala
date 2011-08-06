package rl

import java.net.IDN

object UserInfo {
  def apply(userInfo: String): Option[UserInfo] = {
    userInfo.toOption map { uif ⇒
      val Array(user, secret) = if (uif.indexOf(":") > -1) (uif.toString split ':') else Array(uif, "")
      UserInfo(user, secret)
    }
  }
}
case class UserInfo(user: String, secret: String) extends UriNode {
  val uriPart = toString + "@"
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
      Authority(uinf, HostName(h), Some(port.toInt))
    } else Authority(uinf, HostName(au), None)
  }
}

sealed trait UriHost extends UriNode {
  def value: String
}

case object EmptyHost extends UriHost {
  val uriPart = value

  val value = "/"
}

case class HostName(value: String) extends UriHost {
  val uriPart = UrlCodingUtils.ensureUrlEncoding(IDN.toASCII(value))
}
case class IPv4Address(value: String) extends UriHost {
  val uriPart = value
}
case class IPv6Address(value: String) extends UriHost {
  val uriPart = "[" + value + "]"
}
case class IPvFutureAddress(value: String) extends UriHost {
  val uriPart = "[" + value + "]"
}

case class Authority(userInfo: Option[UserInfo], host: UriHost, port: Option[Int]) extends UriNode {

  val uriPart = "//" + toString + "/"
  override def toString = {
    (userInfo map { _.uriPart } getOrElse "") + host.uriPart + (port map { ":" + _ } getOrElse "") //expresses intent better
  }
}