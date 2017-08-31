name := "stub-data-generator"

organization := "uk.gov.hmrc"

scalaVersion := "2.12.2"

version := "0.4.0"

crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.2")

homepage := Some(url("https://github.com/hmrclt/stub-data-generator"))

libraryDependencies ++= Seq(
  "org.scalacheck"       %% "scalacheck" % "1.13.5",
  "com.github.mpilquist" %% "simulacrum" % "0.11.0",
  "com.chuusai"          %% "shapeless"  % "2.3.2",
  "org.typelevel"        %% "cats"       % "0.9.0",
  "org.scalatest"        %% "scalatest"  % "3.0.3"   % "test",
  "io.github.amrhassan" %% "scalacheck-cats" % "0.3.2"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

initialCommands in console := """import org.scalacheck._; import uk.gov.hmrc.smartstub._"""

enablePlugins(TutPlugin)

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

useGpg := true

scmInfo := Some(
  ScmInfo(
    url("https://github.com/hmrclt/stub-data-generator"),
    "scm:git@github.com:hmrclt/stub-data-generator.git"
  )
)

developers := List(
  Developer(
    id            = "hmrclt",
    name	  = "Luke Tebbs",
    email         = "luke.tebbs@digital.hmrc.gov.uk",
    url           = url("http://www.luketebbs.com/")
  )
)

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}
