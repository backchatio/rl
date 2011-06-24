
package object rl {

  class UriStringExtensions(source: String) {
    def isNotBlank = Option(source) forall { !_.trim.isEmpty }
    def toOption = if (isNotBlank) Some(source) else None
  }

  implicit def string2UriStringExtension(source: String) = new UriStringExtensions(source)
}