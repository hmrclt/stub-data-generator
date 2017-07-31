name := "stub-data-generator"

organization := "uk.gov.hmrc"

scalaVersion := "2.12.2"

version := "0.3.1"

crossScalaVersions := Seq("2.11.11", "2.12.2")

libraryDependencies ++= Seq(
  "org.scalacheck"       %% "scalacheck" % "1.13.5",
  "com.github.mpilquist" %% "simulacrum" % "0.10.0",
  "com.chuusai"          %% "shapeless"  % "2.3.2",
  "org.typelevel"        %% "cats"       % "0.9.0",
  "io.github.amrhassan"  %% "scalacheck-cats" % "0.3.2",
  "org.scalatest"        %% "scalatest"  % "3.0.3"   % "test"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

initialCommands in console := """import org.scalacheck._; import hmrc.smartstub._"""

enablePlugins(TutPlugin)

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))
