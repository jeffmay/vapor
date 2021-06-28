import sbt._

object Dependencies {

  final val Scala_2_13 = "2.13.6"

  private final val catsVersion = "2.6.1"
  private final val kindProjectorVersion = "0.13.0"
  private final val scalaCheckVersion = "1.15.4"
  private final val scalaCheckOpsVersion = "2.6.0"
  private final val scalaTestVersion = "3.2.9"
  private final val scalaTestPlusScalaCheckVersion = "3.2.9.0"
  private final val shapelessVersion = "2.3.7"

  private val alleyCatsCore = "org.typelevel" %% "alleycats-core" % catsVersion
  private val catsCore = "org.typelevel" %% "cats-core" % catsVersion
  private val catsFree = "org.typelevel" %% "cats-free" % catsVersion
  private val scalaCheck = "org.scalacheck" %% "scalacheck" % scalaCheckVersion
  private val scalaCheckOps = "com.rallyhealth" %% "scalacheck-ops_1-15" % scalaCheckOpsVersion
  private val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  private val scalaTestPlusScalaCheck = "org.scalatestplus" %% "scalacheck-1-15" % scalaTestPlusScalaCheckVersion
  private def scalaReflect(scalacVersion: String): ModuleID = "org.scala-lang" % "scala-reflect" % scalacVersion
  private val shapeless = "com.chuusai" %% "shapeless" % shapelessVersion

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
        scalaReflect(scalaVersion),
        shapeless,
      ) ++ Seq(
        // Test-only dependencies
        scalaCheck,
        scalaCheckOps,
        scalaTest,
        scalaTestPlusScalaCheck,
      ).map(_ % Test)
  }

}
