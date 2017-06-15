scodecModule := "scodec-protocols"

scodecPrimaryModule
scodecPrimaryModuleJvm

crossScalaVersions := crossScalaVersions.value.filter { v => v.startsWith("2.11.") || v.startsWith("2.12.") }

contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"))

rootPackage := "scodec.protocols"

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-core" % "1.10.3",
  "org.scodec" %% "scodec-stream" % "1.1.0-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

libraryDependencies ++= {
  if (scalaBinaryVersion.value startsWith "2.10") Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)) else Nil
}

OsgiKeys.exportPackage := Seq("!scodec.bits,!scodec.codecs,!scodec.stream,scodec.protocols.*;version=${Bundle-Version}")

OsgiKeys.importPackage := Seq(
  """scodec.*;version="$<range;[==,=+);$<@>>"""",
  """scala.*;version="$<range;[==,=+);$<@>>"""",
  """fs2.*;version="$<range;[==,=+);$<@>>"""",
  """shapeless.*;version="$<range;[==,=+);$<@>>"""",
  "*"
)

parallelExecution in Test := false
