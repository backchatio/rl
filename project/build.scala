import sbt._
import Keys._
import scala.xml._
//import com.typesafe.sbtscalariform._
//import ScalariformPlugin._
//import ScalariformKeys._

// Shell prompt which show the current project, git branch and build version
// git magic from Daniel Sobral, adapted by Ivan Porto Carrero to also work with git flow branches
object ShellPrompt {
 
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }
  
  val current = """\*\s+([^\s]+)""".r
  
  def gitBranches = ("git branch --no-color" lines_! devnull mkString)
  
  val buildShellPrompt = { 
    (state: State) => {
      val currBranch = current findFirstMatchIn gitBranches map (_ group(1)) getOrElse "-"
      val currProject = Project.extract (state).currentProject.id
      "%s:%s:%s> ".format (currBranch, currProject, RlSettings.buildVersion)
    }
  }
 
}

object RlSettings {
  val buildOrganization = "io.backchat.rl"
  val buildScalaVersion = "2.9.2"
  val buildVersion      = "0.3.3-SNAPSHOT"
//
//  lazy val formatSettings = ScalariformPlugin.scalariformSettings ++ Seq(
//     preferences in Compile := formattingPreferences,
//     preferences in Test    := formattingPreferences
//  )
//
//  def formattingPreferences = {
//     import scalariform.formatter.preferences._
//     (FormattingPreferences()
//         setPreference(IndentSpaces, 2)
//         setPreference(AlignParameters, true)
//         setPreference(AlignSingleLineCaseStatements, true)
//         setPreference(DoubleIndentClassDeclaration, true)
//         setPreference(RewriteArrowSymbols, true)
//         setPreference(PreserveSpaceBeforeArguments, true))
//  }

  val description = SettingKey[String]("description")

  val compilerPlugins = Seq(
    compilerPlugin("org.scala-lang.plugins" % "continuations" % buildScalaVersion)//,
    // compilerPlugin("org.scala-tools.sxr" % "sxr_2.9.0" % "0.2.7")
  )

  val buildSettings = Defaults.defaultSettings ++ Seq(
      name := "rl",
      version := buildVersion,
      organization := buildOrganization,
      scalaVersion := buildScalaVersion,
      javacOptions ++= Seq("-Xlint:unchecked"),
      exportJars := true,
      testOptions in Test += Tests.Setup( () => System.setProperty("akka.mode", "test") ),
      scalacOptions ++= Seq(
        "-optimize",
        "-deprecation",
        "-unchecked",
        "-Xcheckinit",
        "-encoding", "utf8",
        "-P:continuations:enable"),
      libraryDependencies <+= (scalaVersion) {
        case "2.9.0-1" => "org.specs2" %% "specs2" % "1.5" % "test"
        case "2.9.0" => "org.specs2" % "specs2_2.9.0-1" % "1.5" % "test"
        case _ => "org.specs2" %% "specs2" % "1.12" % "test"
      },
      libraryDependencies += "junit" % "junit" % "4.10" % "test",
      resolvers ++= Seq(
        "ScalaTools Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
        "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
      ),
      crossScalaVersions := Seq("2.9.1", "2.9.0-1", "2.9.0", "2.9.1-1", "2.9.2"),
//      (excludeFilter in format) <<= (excludeFilter) (_ || "*Spec.scala"),
      libraryDependencies ++= compilerPlugins,
      artifact in (Compile, packageBin) ~= { (art: Artifact) =>
        if (sys.props("java.version") startsWith "1.7") art.copy(classifier = Some("jdk17")) else art
      },
      autoCompilerPlugins := true,
      parallelExecution in Test := false,
      shellPrompt  := ShellPrompt.buildShellPrompt,
      testOptions := Seq(
        Tests.Argument("console", "junitxml")),
      testOptions <+= crossTarget map { ct =>
        Tests.Setup { () => System.setProperty("specs2.junit.outDir", new File(ct, "specs-reports").getAbsolutePath) }
      }) //++ formatSettings

  val packageSettings = Seq (
    packageOptions <<= (packageOptions, name, version, organization) map {
      (opts, title, version, vendor) =>
         opts :+ Package.ManifestAttributes(
          "Created-By" -> "Simple Build Tool",
          "Built-By" -> System.getProperty("user.name"),
          "Build-Jdk" -> System.getProperty("java.version"),
          "Specification-Title" -> title,
          "Specification-Vendor" -> "Mojolly Ltd.",
          "Specification-Version" -> version,
          "Implementation-Title" -> title,
          "Implementation-Version" -> version,
          "Implementation-Vendor-Id" -> vendor,
          "Implementation-Vendor" -> "Mojolly Ltd.",
          "Implementation-Url" -> "https://backchat.io"
         )
    },
    homepage := Some(url("https://backchat.io")),
    startYear := Some(2010),
    licenses := Seq(("MIT", url("http://github.com/mojolly/rl/raw/HEAD/LICENSE"))),
    pomExtra <<= (pomExtra, name, description) {(pom, name, desc) => pom ++ Group(
      <scm>
        <connection>scm:git:git://github.com/mojolly/rl.git</connection>
        <developerConnection>scm:git:git@github.com:mojolly/rl.git</developerConnection>
        <url>https://github.com/mojolly/rl</url>
      </scm>
      <developers>
        <developer>
          <id>casualjim</id>
          <name>Ivan Porto Carrero</name>
          <url>http://flanders.co.nz/</url>
        </developer>
        <developer>
          <id>ben-biddington</id>
          <name>Ben Biddington</name>
          <url>http://benbiddington.wordpress.com/</url>
        </developer>
      </developers>
    )},
    publishMavenStyle := true,
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    pomIncludeRepository := { x => false })

  val projectSettings = buildSettings ++ packageSettings
}

object RlBuild extends Build {

  import RlSettings._
  val buildShellPrompt =  ShellPrompt.buildShellPrompt
  object rl {
    val downloadDomainFile = TaskKey[Int]("update-tld-file", "updates the tld names dat file from mozilla")
    val domainFile = SettingKey[File]("tld-file", "the file that contains the tld names")
    val domainFileUrl = SettingKey[URL]("tld-file-url", "the url from where to download the file that contains the tld names")
  }

  lazy val root = Project ("rl", file("."), settings = projectSettings ++ Seq(
    rl.domainFile <<= (resourceDirectory in Compile) apply { dir =>
      val rlResource = dir / "rl"
      rlResource.mkdirs()
      rlResource / "tld_names.dat"
    },
    rl.domainFileUrl := url("http://mxr.mozilla.org/mozilla-central/source/netwerk/dns/effective_tld_names.dat?raw=1"),
    rl.downloadDomainFile <<= (rl.domainFile, rl.domainFileUrl, streams) map (_ #< _ ! _.log),
    (compile in Compile) <<= (compile in Compile) dependsOn rl.downloadDomainFile,
    description := "An RFC-3986 compliant URI library."))
  
}

// vim: set ts=2 sw=2 et:

