package rl

object UriFragment {
  def apply(rawValue: String) = rawValue.blankOpt map { StringFragment(_) } getOrElse EmptyFragment
}
trait UriFragment extends UriNode {
  type Value
  def rawValue: String
  def value: Value

  def normalize: UriFragment = this
}

case object EmptyFragment extends UriFragment {
  val uriPart = ""

  val value = ""

  val rawValue = ""

  type Value = String
}
case class StringFragment(rawValue: String) extends UriFragment {
  def uriPart = value.blankOpt map { "#" + _ } getOrElse ""

  val value = rawValue

  type Value = String
}
