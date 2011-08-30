package rl

import java.util.Locale.ENGLISH

trait UriScheme extends UriNode {
  def scheme: String
}
case object NoScheme extends UriScheme {
  val scheme = ""
  val uriPart = scheme

  def normalize = this
}
object Scheme {
  val DEFAULT_PORTS = Map(
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
    "mongo" -> 27017)
}
case class Scheme(scheme: String) extends UriScheme {
  val uriPart = scheme.toOption map { _ + ":" } getOrElse ""

  def normalize = copy(scheme.toLowerCase(ENGLISH))
}