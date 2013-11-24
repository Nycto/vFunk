name := "vFunk"

organization := "com.roundeights"

version := "0.1"

scalaVersion := "2.10.3"

// Compiler flags
scalacOptions ++= Seq("-deprecation", "-feature")

// Repositories in which to find dependencies
resolvers ++= Seq(
    "Specs Repository" at "http://oss.sonatype.org/content/repositories/releases"
)

publishTo := Some("Spikemark" at "https://spikemark.herokuapp.com/maven/roundeights")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

// Application dependencies
libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.3.4" % "test"
)
