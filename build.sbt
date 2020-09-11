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

def commonProject(dir: String): Project = {
  Project(dir, file(dir)).settings(
    name := s"vapors-$dir",
  )
}

lazy val core = commonProject("core")
  .settings(
    libraryDependencies ++= Dependencies.CoreProject.all(scalaVersion.value),
  )

lazy val circe = commonProject("circe")
  .dependsOn(core)
  .settings(
    libraryDependencies ++= Dependencies.CirceProject.all,
  )
