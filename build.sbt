scodecModule := "scodec-protocols"

scodecPrimaryModule

contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"))

rootPackage := "scodec.protocols"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-stream" % "0.7.1",
  "org.scodec" %% "scodec-scalaz" % "1.0.0",
  "joda-time" % "joda-time" % "2.6",
  "org.joda" % "joda-convert" % "1.7",
  "org.scalatest" %% "scalatest" % "2.2.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.0" % "test"
)

libraryDependencies ++= {
  if (scalaBinaryVersion.value startsWith "2.10") Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)) else Nil
}

OsgiKeys.exportPackage := Seq("!scodec.bits,!scodec.codecs,scodec.protocols.*;version=${Bundle-Version}")

OsgiKeys.importPackage := Seq(
  """scodec.*;version="$<range;[==,=+);$<@>>"""",
  """scala.*;version="$<range;[==,=+);$<@>>"""",
  """scalaz.*;version="$<range;[==,=+);$<@>>"""",
  "*"
)

