// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.0" // your current series x.y
ThisBuild / scalafixDependencies += "org.typelevel" %% "simulacrum-scalafix" % "0.5.3"

ThisBuild / organization := "com.github.fabianmurariu"
ThisBuild / organizationName := "32Bytes Software LTD"
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("fabianmurariu", "Fabian Murariu")
)

// publish to s01.oss.sonatype.org (set to true to publish to oss.sonatype.org instead)
ThisBuild / tlSonatypeUseLegacyHost := false

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")

val Scala213 = "2.13.8"
val Scala3 = "3.1.1"
ThisBuild / crossScalaVersions := Seq(Scala213, Scala3)
ThisBuild / scalaVersion := Scala213 // the default Scala

lazy val root = tlCrossRootProject.aggregate(core, benchmarks)

lazy val simulacrumSettings = Seq(
  libraryDependencies ++= (if (tlIsScala3.value) Nil
                           else Seq(compilerPlugin(scalafixSemanticdb))),
  scalacOptions ++= (
    if (tlIsScala3.value) Nil
    else
      Seq(
        s"-P:semanticdb:targetroot:${baseDirectory.value}/target/.semanticdb",
        "-Yrangepos"
      )
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq("-Ykind-projector:underscores")
      case Some((2, 12 | 13)) =>
        Seq("-Xsource:3", "-P:kind-projector:underscore-placeholders")
    }
  },
  libraryDependencies += "org.typelevel" %% "simulacrum-scalafix-annotations" % "0.5.4"
)

lazy val macroSettings = Seq(
  libraryDependencies ++= {
    if (tlIsScala3.value)
      Nil
    else
      Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided)
  }
)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(simulacrumSettings, macroSettings)
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.7.0",
      "org.typelevel" %%% "cats-free" % "2.7.0",
      "org.typelevel" %%% "cats-effect" % "3.3.11",
      "org.scalameta" %%% "munit" % "0.7.29" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "0.7.29" % Test,
      "org.typelevel" %%% "munit-cats-effect-3" % "1.0.7" % Test,
      "org.typelevel" %%% "scalacheck-effect-munit" % "1.0.4" % Test
    )
  )

lazy val benchmarks = crossProject(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("benchmarks"))
  .settings(simulacrumSettings, macroSettings)
  .enablePlugins(JmhPlugin)
  .dependsOn(core)
  .settings(
    name := "benchmarks"
  )

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin)
