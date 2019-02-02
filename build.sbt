scodecModule := "scodec-protocols"

enablePlugins(ScodecPrimaryModuleSettings)
enablePlugins(ScodecPrimaryModuleJVMSettings)

crossScalaVersions := crossScalaVersions.value.filter { v => v.startsWith("2.11.") || v.startsWith("2.12.") }
releaseCrossBuild := true

contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"))

rootPackage := "scodec.protocols"
scmInfo := Some(ScmInfo(url("https://github.com/scodec/scodec-protocols"), "git@github.com:scodec/scodec-protocols.git"))

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-core" % "1.11.2",
  "org.scodec" %% "scodec-stream" % "1.2.1-SNAPSHOT",
  "co.fs2" %% "fs2-core" % "1.0.3",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"
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
