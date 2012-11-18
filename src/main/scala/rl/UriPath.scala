package rl

import collection.GenSeq
import collection.immutable.Vector

trait UriPath extends UriNode {
  def segments: GenSeq[String]
  def isRelative: Boolean
  def isAbsolute: Boolean

  def collapseDots(): GenSeq[String] = {
    (Vector.empty[String] /: segments) { (lb, seg) ⇒
      seg match {
        case "."  ⇒ lb
        case ".." ⇒ if (!lb.isEmpty) lb.dropRight(1) else lb
        case a    ⇒ lb :+ a
      }
    }
  }

  def normalize: UriPath

  def apply() = uriPart
}

trait EmptyUriPath extends UriPath {
  val segments = Nil

  def normalize = this
}

case object EmptyPath extends EmptyUriPath {
  val isAbsolute: Boolean = false

  val isRelative: Boolean = true

  val uriPart = "/"
}

case class RelativePath(segments: GenSeq[String]) extends UriPath {
  val isAbsolute: Boolean = false

  val isRelative: Boolean = true

  val uriPart = segments map { UrlCodingUtils.ensureUrlEncoding(_) } mkString ("", UriPath.unixSeparator, UriPath.unixSeparator)

  def normalize = RelativePath(collapseDots())
}
case class AbsolutePath(segments: GenSeq[String]) extends UriPath {
  val isAbsolute: Boolean = true

  val isRelative: Boolean = false

  val uriPart = toUriPart

  def normalize = AbsolutePath(collapseDots())

  private def toUriPart = {
    if (segments.size == 0) "" else
      segments map { UrlCodingUtils.ensureUrlEncoding(_) } mkString (UriPath.unixSeparator,
        UriPath.unixSeparator,
        UriPath.unixSeparator)
  }
}
trait PathOps {

  private val wlpExpr = """^[A-Za-z]:\\""".r
  private val wuncpExpr = """^\\\\""".r
  val windowsSeparator = "\\"
  val unixSeparator = "/"

  def windowsToUnixPath(path: String) = {
    if (wlpExpr.findFirstIn(path).isDefined) {
      "file:///" + convertWindowsToUnixPath(path)
    } else if (wuncpExpr.findFirstIn(path).isDefined) {
      "file:" + convertWindowsToUnixPath(path)
    } else convertWindowsToUnixPath(path)
  }

  private def convertWindowsToUnixPath(path: String) = {
    path.replace(windowsSeparator, unixSeparator).replace(" ", "%20")
  }
}

object UriPath extends PathOps
