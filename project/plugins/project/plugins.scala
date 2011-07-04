import sbt._
import Keys._

object Plugins extends Build {

  lazy val formatterPlugin = ProjectRef( uri("file:///Users/ivan/src/scala/sbt-cool-plugins"), "Formatter")

  lazy override val projects = Seq(root)
  lazy val root = Project(
    "plugins",
    file("."),
    settings = Defaults.defaultSettings ++ Seq(resolvers += "ScalaTools Snapshots" at "http://maven.mojolly.com/content/repositories/thirdparty-snapshots/")
    ) dependsOn (formatterPlugin)
}