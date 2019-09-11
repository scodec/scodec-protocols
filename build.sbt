scodecModule := "scodec-protocols"

enablePlugins(ScodecPrimaryModuleSettings)
enablePlugins(ScodecPrimaryModuleJVMSettings)

releaseCrossBuild := true

contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"))

rootPackage := "scodec.protocols"
scmInfo := Some(ScmInfo(url("https://github.com/scodec/scodec-protocols"), "git@github.com:scodec/scodec-protocols.git"))

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-core" % "1.11.4",
  "org.scodec" %% "scodec-stream" % "2.0.0",
  "co.fs2" %% "fs2-core" % "2.0.0",
  "co.fs2" %% "fs2-io" % "2.0.0" % "test",
  "org.scalatest" %% "scalatest" % "3.1.0-RC2" % "test",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

libraryDependencies ++= {
  if (scalaBinaryVersion.value startsWith "2.10") Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)) else Nil
}

scalacOptions --= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) if v >= 13 =>
      Seq("-Yno-adapted-args", "-Ywarn-unused-import")
    case _ =>
      Seq()
  }
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
