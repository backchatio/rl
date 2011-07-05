package rl

trait QueryString extends UriNode {
  type Value
  def rawValue: String
  def value: Value
  def empty: Value
}
case object EmptyQueryString extends QueryString {

  def empty = ""

  type Value = String
  val value = empty
  val uriPart = empty
  val rawValue = empty
}
case class StringQueryString(rawValue: String) extends QueryString {

  val uriPart = "?" + rawValue.urlEncode
  val value = rawValue.urlDecode

  val empty = ""

  type Value = String
}
case class StringSeqQueryString(rawValue: String) extends QueryString {
  val uriPart = "?" + value.sortWith(_ >= _).map(_.urlEncode).mkString("?", "&", "")

  val empty = Nil

  val value: Value = rawValue.split("&").map(_.urlDecode).toList

  type Value = List[String]
}
case class MapQueryString(rawValue: String) extends QueryString {
  val uriPart = {
    "?" + (value map {
      case (k, v) ⇒ v.map(s ⇒ "%s=%s".format(k.urlEncode, s.urlEncode)).mkString("&")
    } mkString "&")
  }

  val empty = Map.empty[String, List[String]]

  lazy val value = parseRaw()

  private def parseRaw() = {
    if (rawValue.indexOf('&') > -1) {
      rawValue.split('&').foldRight(Map[String, List[String]]()) { readQsPair _ }
    } else {
      readQsPair(rawValue)
    }
  }
  private def readQsPair(pair: String, current: Map[String, List[String]] = Map.empty) = {
    (pair split '=' toList) map { _.urlDecode } match {
      case item :: Nil ⇒ current + (item -> List[String]())
      case item :: rest ⇒
        if (!current.contains(item)) current + (item -> rest) else (current + (item -> (rest ::: current(item)).distinct))
      case _ ⇒ current
    }
  }
  type Value = Map[String, List[String]]
}