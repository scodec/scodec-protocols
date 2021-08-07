addCommandAlias("fmt", "; compile:scalafmt; test:scalafmt; scalafmtSbt")
addCommandAlias("fmtCheck", "; compile:scalafmtCheck; test:scalafmtCheck; scalafmtSbtCheck")

ThisBuild / baseVersion := "3.0"

ThisBuild / organization := "org.scodec"
ThisBuild / organizationName := "Scodec"

ThisBuild / homepage := Some(url("https://github.com/scodec/scodec-protocols"))
ThisBuild / startYear := Some(2013)

ThisBuild / crossScalaVersions := Seq("3.0.1")

ThisBuild / strictSemVer := false

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8")

ThisBuild / spiewakMainBranches := List("main")

ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/scodec/scodec-protocols"), "git@github.com:scodec/scodec-protocols.git")
)

ThisBuild / licenses := List(
  ("BSD-3-Clause", url("https://github.com/scodec/scodec-protocols/blob/main/LICENSE"))
)

ThisBuild / testFrameworks += new TestFramework("munit.Framework")

ThisBuild / publishGithubUser := "mpilquist"
ThisBuild / publishFullName := "Michael Pilquist"

ThisBuild / fatalWarningsInCI := false

ThisBuild / mimaBinaryIssueFilters ++= Seq(
)

val core = project
  .in(file("."))
  .enablePlugins(SonatypeCiReleasePlugin, SbtOsgi)
  .settings(
    name := "scodec-protocols",
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-stream" % "3.0.1",
      "co.fs2" %%% "fs2-io" % "3.1.0" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "0.7.27" % Test
    ),
    Compile / unmanagedResources ++= {
      val base = baseDirectory.value
      (base / "NOTICE") +: (base / "LICENSE") +: ((base / "licenses") * "LICENSE_*").get

    },
    OsgiKeys.privatePackage := Nil,
    OsgiKeys.exportPackage := Seq("scodec.stream.*;version=${Bundle-Version}"),
    OsgiKeys.importPackage := Seq(
      """scala.*;version="$<range;[==,=+);$<@>>"""",
      """fs2.*;version="$<range;[==,=+);$<@>>"""",
      """scodec.*;version="$<range;[==,=+);$<@>>"""",
      "*"
    ),
    OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
  )

