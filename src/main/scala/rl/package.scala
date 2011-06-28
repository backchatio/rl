
package object rl {

  class UriStringExtensions(source: String) {
    def isNotBlank = Option(source) forall { !_.trim.isEmpty }
    def toOption = if (isNotBlank) Some(source) else None
  }

  implicit def string2UriStringExtension(source: String) = new UriStringExtensions(source)

  val IPv6Address =
    ("""(?iu)^(((?=(?>.*?::)(?!.*::)))(::)?([0-9A-F]{1,4}::?){0,5}|([0-9A-F]{1,4}:){6})(\2([0-9A-F]{1,4}(::?|$)){0,2}""" +
        """|((25[0-5]|(2[0-4]|1[0-9]|[1-9])?[0-9])(\.|$)){4}|[0-9A-F]{1,4}:[0-9A-F]{1,4})(?<![^:]:)(?<!\.)\z""").r

}