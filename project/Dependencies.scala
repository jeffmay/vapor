import sbt._

object Dependencies {

  final val Scala_2_13 = "2.13.0"

  private final val catsVersion = "2.3.0"
  private final val circeVersion = "0.13.0"
  private final val kindProjectorVersion = "0.11.0"
  private final val scalaCheckVersion = "1.15.1"
  private final val scalaCheckOpsVersion = "2.5.1"
  private final val scalaTestVersion = "3.2.2"
  private final val scalaTestPlusScalaCheckVersion = "3.1.4.0"

  private val alleyCatsCore = "org.typelevel" %% "alleycats-core" % catsVersion
  private val catsCore = "org.typelevel" %% "cats-core" % catsVersion
  private val catsFree = "org.typelevel" %% "cats-free" % catsVersion
  private val circeCore = "io.circe" %% "circe-core" % circeVersion
  private val scalaCheck = "org.scalacheck" %% "scalacheck" % scalaCheckVersion
  private val scalaCheckOps = "com.rallyhealth" %% "scalacheck-ops_1-14" % scalaCheckOpsVersion
  private val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  private val scalaTestPlusScalaCheck = "org.scalatestplus" %% "scalacheck-1-14" % scalaTestPlusScalaCheckVersion
  private def scalaReflect(scalacVersion: String): ModuleID = "org.scala-lang" % "scala-reflect" % scalacVersion

  final object Plugins {

    val kindProjector = {
      compilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)
    }
  }

  final object CoreProject {

    def all(scalaVersion: String): Seq[ModuleID] =
      Seq(
        Plugins.kindProjector,
        alleyCatsCore,
        catsCore,
        catsFree,
        circeCore,
        scalaReflect(scalaVersion),
      ) ++ Seq(
        // Test-only dependencies
        scalaCheck,
        scalaCheckOps,
        scalaTest,
        scalaTestPlusScalaCheck,
      ).map(_ % Test)
  }

}
