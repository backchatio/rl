package rl

import collection.GenSeq

trait UriPath extends UriNode {
  def segments: GenSeq[String]
  def isRelative: Boolean
  def isAbsolute: Boolean

  def /(path: String): Uri = /(Uri(path))
  def /(uri: Uri): Uri = null

  def merge(uri: Uri): Uri = null

  def relativize(uri: Uri): Uri = null
}

trait EmptyUriPath extends UriPath {
  val segments = Nil
}

case object EmptyPath extends EmptyUriPath {
  val isAbsolute: Boolean = false

  val isRelative: Boolean = true

  val uriPart = ""
}

case class RelativePath(segments: GenSeq[String]) extends UriPath {
  val isAbsolute: Boolean = false

  val isRelative: Boolean = true

  val uriPart = segments mkString ("", UriPath.unixSeparator, UriPath.unixSeparator)
}
case class AbsolutePath(segments: GenSeq[String]) extends UriPath {
  val isAbsolute: Boolean = true

  val isRelative: Boolean = false

  val uriPart = segments mkString ("", UriPath.unixSeparator, UriPath.unixSeparator)
}
trait PathOps {

  private val wlpExpr = """^[A-Za-z]:\\""".r
  private val wuncpExpr = """^\\\\""".r
  val windowsSeparator = "\\".intern
  val unixSeparator = "/".intern

  def windowsToUnixPath(path: String) = {
    if (wlpExpr.findFirstIn(path).isDefined) {
      "file:///" + convertWindowsToUnixPath(path)
    } else if (wuncpExpr.findFirstIn(path).isDefined) {
      "file:" + convertWindowsToUnixPath(path)
    } else convertWindowsToUnixPath(path)
  }

  private def convertWindowsToUnixPath(path: String) = path.replace(windowsSeparator, unixSeparator)

}

object UriPath extends PathOps