scodecModule := "scodec-protocols"

scodecPrimaryModule

contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"))

rootPackage := "scodec.protocols"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.typelevel" %% "scodec-stream" % "0.6.0",
  "joda-time" % "joda-time" % "2.6",
  "org.joda" % "joda-convert" % "1.7",
  "org.scalatest" %% "scalatest" % "2.2.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.0" % "test"
)

OsgiKeys.exportPackage := Seq("!scodec.bits,!scodec.codecs,scodec.protocols.*;version=${Bundle-Version}")

OsgiKeys.importPackage := Seq(
  """scodec.*;version="$<range;[==,=+);$<@>>"""",
  """scala.*;version="$<range;[==,=+);$<@>>"""",
  """scalaz.*;version="$<range;[==,=+);$<@>>"""",
  "*"
)

