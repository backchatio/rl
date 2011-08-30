package rl

object UriFragment {
  def apply(rawValue: String) = rawValue.toOption map { StringFragment(_) } getOrElse EmptyFragment
}
trait UriFragment extends UriNode {
  type Value
  def rawValue: String
  def value: Value

  def normalize = this
}

case object EmptyFragment extends UriFragment {
  val uriPart = ""

  val value = ""

  val rawValue = ""

  type Value = String
}
case class StringFragment(rawValue: String) extends UriFragment {
  def uriPart = value.toOption map { "#" + _ } getOrElse ""

  val value = rawValue

  type Value = String
}
