name := "vFunk"

scalaVersion := "2.9.2"

version := "0.1"

// append -deprecation to the options passed to the Scala compiler
scalacOptions += "-deprecation"

// Repositories in which to find dependencies
resolvers ++= Seq(
    "Maven Repository" at "http://repo1.maven.org/maven2/org/",
    "Specs Repository" at "http://oss.sonatype.org/content/repositories/releases"
)

// Application dependencies
libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.12.1" % "test",
    "org.mockito" % "mockito-all" % "1.9.5-rc1" % "test"
)
