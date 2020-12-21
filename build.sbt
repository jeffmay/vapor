name := "vapors-root"
ThisBuild / organization := "com.rallyhealth"
ThisBuild / organizationName := "Rally Health"

ThisBuild / licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

ThisBuild / scalaVersion := Dependencies.Scala_2_13

ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-opt-warnings:at-inline-failed",
  "-Xfatal-warnings",
  "-Ymacro-annotations",
)

ThisBuild / bintrayOrganization := Some("rallyhealth")
ThisBuild / bintrayRepository := "maven"

ThisBuild / resolvers += Resolver.bintrayRepo("rallyhealth", "maven")

// ScalaDoc generation is generally broken. It's really mostly useful from within the IDE anyway
// so just disable generation to allow publishing without ScalaDoc errors.
ThisBuild / Compile / packageDoc / publishArtifact := false
ThisBuild / packageDoc / publishArtifact := false

def commonProject(dir: String): Project = {
  Project(dir, file(dir)).settings(
    name := s"vapors-$dir",
  )
}

lazy val core = commonProject("core")
  .settings(
    libraryDependencies ++= Dependencies.CoreProject.all(scalaVersion.value),
  )
