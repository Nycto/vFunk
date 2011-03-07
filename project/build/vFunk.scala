import sbt._

class VFunkProject(info: ProjectInfo) extends DefaultProject(info)
{
    val specs = "specs" % "specs" % "1.6.7" from
        "http://specs.googlecode.com/files/specs_2.8.1-1.6.7.jar"

    val mockito = "mockito" % "mockito" % "1.8.5" from
        "http://mockito.googlecode.com/files/mockito-all-1.8.5.jar"

    val junit = "junit" % "junit" % "4.8.2" from
        "http://cloud.github.com/downloads/KentBeck/junit/junit-4.8.2.jar"
}

