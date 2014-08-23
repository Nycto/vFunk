name := "vFunk"

organization := "com.roundeights"

version := "1.1.0"

scalaVersion := "2.11.2"

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
    "org.specs2" %% "specs2" % "2.3.11" % "test"
)
