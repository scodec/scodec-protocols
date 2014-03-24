import sbtrelease._
import ReleaseStateTransformations._
import ReleasePlugin._
import ReleaseKeys._

organization := "org.typelevel"

name := "scodec-protocols"

scalaVersion := "2.10.3"

crossScalaVersions := Seq(scalaVersion.value)

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-optimise",
  "-Xcheckinit",
  "-Xlint",
  "-Xverify",
  "-Yclosure-elim",
  "-Yinline",
  "-Yno-adapted-args",
  "-Ywarn-all")

scalacOptions in (Compile, doc) ++= {
  val tagOrBranch = if (version.value endsWith "SNAPSHOT") "master" else ("v" + version.value)
  Seq(
    "-diagrams",
    "-groups",
    "-implicits",
    "-implicits-show-all",
    "-skip-packages", "scalaz",
    "-sourcepath", baseDirectory.value.getAbsolutePath,
    "-doc-source-url", "https:///github.com/scodec/scodec-protocols/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
  )
}


licenses += ("Three-clause BSD-style", url("http://github.com/scodec/scodec-protocols/blob/master/LICENSE"))

unmanagedResources in Compile <++= baseDirectory map { base => (base / "NOTICE") +: (base / "LICENSE") +: ((base / "licenses") * "LICENSE_*").get }

triggeredMessage := (_ => Watched.clearScreen)

resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

libraryDependencies ++= Seq(
  "org.typelevel" %% "scodec-core" % "1.0.0-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)

osgiSettings

OsgiKeys.exportPackage := Seq("!scodec.bits,!scodec.codecs,scodec.codecs.*;version=${Bundle-Version}")

OsgiKeys.importPackage := Seq(
  """scodec.*;version="$<range;[==,=+);$<@>>"""",
  """scala.*;version="$<range;[==,=+);$<@>>"""",
  """scalaz.*;version="$<range;[==,=+);$<@>>"""",
  "*"
)

OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

pomExtra := (
  <url>http://github.com/scodec/scodec</url>
  <scm>
    <url>git@github.com:scodec/scodec.git</url>
    <connection>scm:git:git@github.com:scodec/scodec-protocols.git</connection>
  </scm>
  <developers>
    <developer>
      <id>mpilquist</id>
      <name>Michael Pilquist</name>
      <url>http://github.com/mpilquist</url>
    </developer>
    <developer>
      <id>pchiusano</id>
      <name>Paul Chiusano</name>
      <url>http://github.com/pchiusano</url>
    </developer>
  </developers>
)

pomPostProcess := { (node) =>
  import scala.xml._
  import scala.xml.transform._
  def stripIf(f: Node => Boolean) = new RewriteRule {
    override def transform(n: Node) =
      if (f(n)) NodeSeq.Empty else n
  }
  val stripTestScope = stripIf { n => n.label == "dependency" && (n \ "scope").text == "test" }
  new RuleTransformer(stripTestScope).transform(node)(0)
}

releaseSettings

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  publishArtifacts.copy(action = publishSignedAction),
  setNextVersion,
  commitNextVersion,
  pushChanges
)

site.settings

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:scodec/scodec-protocols.git"

