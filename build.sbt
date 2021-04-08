name := "vapors-root"
ThisBuild / organization := "com.rallyhealth"
ThisBuild / organizationName := "Rally Health"

ThisBuild / licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

ThisBuild / scalaVersion := Dependencies.Scala_2_13

ThisBuild / scalacOptions ++= Seq(
  "-deprecation:false",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
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

// Disable publishing of the root project
publish := {}
publishLocal := {}

def commonProject(
  dir: String,
  projectPrefix: String = "",
): Project = {
  val packagePrefix = s"com.rallyhealth.vapors${if (projectPrefix.isEmpty) "" else s".$projectPrefix"}"
  Project(dir, file(dir)).settings(
    name := s"vapors-$dir",
    idePackagePrefix := Some(packagePrefix),
  )
}

lazy val core = commonProject("core")
  .settings(
    libraryDependencies ++= Dependencies.CoreProject.all(scalaVersion.value),
  )
