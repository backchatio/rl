package rl

trait PathUtils {

  private val wlpExpr = """^[A-Za-z]:\\""".r
  private val wuncpExpr = """^\\\\""".r
  private val windowsSeparator = "\\".intern
  private val unixSeparator = "/".intern

  def normalizeWindowsPath(path: String) = {
    if (wlpExpr.findFirstIn(path).isDefined) {
      "file:///" + windowsToUnixPath(path)
    } else if (wuncpExpr.findFirstIn(path).isDefined) {
      "file:" + windowsToUnixPath(path)
    } else windowsToUnixPath(path)
  }

  private def windowsToUnixPath(path: String) = path.replace(windowsSeparator, unixSeparator)

}

object PathUtils extends PathUtils