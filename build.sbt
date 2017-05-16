name := "stub-data-generator"

organization := "hmrc"

scalaVersion := "2.12.2"

version := "0.1.0"

crossScalaVersions := Seq("2.11.11", "2.12.2")

libraryDependencies ++= Seq(
  "org.scalacheck"       %% "scalacheck" % "1.13.5",
  "com.github.mpilquist" %% "simulacrum" % "0.10.0",
  "com.chuusai"          %% "shapeless"  % "2.3.2",
  "org.scalatest"        %% "scalatest"  % "3.0.3"   % "test"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

initialCommands in console := """import org.scalacheck._; import hmrc.smartstub._"""

enablePlugins(TutPlugin)

enablePlugins(GhpagesPlugin)

git.remoteRepo := "git@github.com:hmrclt/stub-data-generator.git"
ghpagesNoJekyll := true



