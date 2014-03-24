scodec-protocols
================

Provides models of common binary protocols using the [scodec library](https://github.com/scodec/scodec).

Getting Binaries
----------------

This library works with Scala 2.10.*.

ScalaDoc for the latest version is available here: http://scodec.github.io/scodec-protocols/latest/api.

### Releases

There are no stable releases yet.

For SBT users:

    libraryDependencies += "org.typelevel" %% "scodec-protocols" % "1.0.0-SNAPSHOT"


For Maven users:

    <dependencies>
      <dependency>
        <groupId>org.typelevel</groupId>
        <artifactId>scodec-protocols_2.10</artifactId>
        <version>1.0.0-SNAPSHOT</version>
      </dependency>
    </dependencies>


### Snapshots

Snapshot builds of the master branch are available on Sonatype's OSS hosting at https://oss.sonatype.org/content/repositories/snapshots/.

For SBT users:

    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

    libraryDependencies += "org.typelevel" %% "scodec-protocols" % "1.0.0-SNAPSHOT"


For Maven users:

    <repositories>
      <repository>
        <id>sonatype-oss-snapshots</id>
        <name>Sonatype OSS Snapshots</name>
        <url>https://oss.sonatype.org/content/repositories/snapshots/</url>
      </repository>
    </repositories>

    <dependencies>
      <dependency>
        <groupId>org.typelevel</groupId>
        <artifactId>scodec-protocols_2.10</artifactId>
        <version>1.0.0-SNAPSHOT</version>
      </dependency>
    </dependencies>

Building
--------

This project uses sbt. To build, run `sbt publish-local`.
