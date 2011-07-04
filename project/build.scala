import sbt._
import Keys._
import org.sbtidea._
import SbtIdeaPlugin._
import com.github.oforero.sbtformatter._

object RlBuild extends Build {
  val root = Project ("rl", file("."), settings = Defaults.defaultSettings ++ Seq(
  name := "rl",
  version := "0.0.1-SNAPSHOT",
  organization := "com.mojolly.rl",
  scalaVersion := "2.9.0-1",
  scalacOptions ++= Seq("-optimize", "-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8"),
  libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.4" % "test"
  ),
  testFrameworks += new TestFramework("org.specs2.runner.SpecsFramework"),
  credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
  publishTo <<= (version) { version: String =>
    val nexus = "http://maven.mojolly.com/content/repositories/"
    if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus+"snapshots/")
    else                                   Some("releases" at nexus+"releases/")
  },
  ideaProjectName := "rl"))
}
