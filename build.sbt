import Dependencies._

ThisBuild / organization := "com.github.tkrs"
ThisBuild / scalaVersion := Ver.`scala2.13`
ThisBuild / crossScalaVersions := Seq(
  Ver.`scala2.12`,
  Ver.`scala2.13`
)
ThisBuild / resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
ThisBuild / libraryDependencies ++= Seq(Pkg.scalatest, Pkg.scalacheck).map(_ % Test)
ThisBuild / scalacOptions ++= compilerOptions ++ warnCompilerOptions ++ {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) if n >= 13 => Nil
    case _                       => Seq("-Xfuture", "-Ypartial-unification", "-Yno-adapted-args")
  }
}
ThisBuild / scalafmtOnCompile := true

lazy val compilerOptions = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-unchecked",
  "-language:higherKinds",
  "-feature"
)

lazy val warnCompilerOptions = Seq(
  "-Xlint",
  // "-Xfatal-warnings",
  "-Ywarn-extra-implicit",
  "-Ywarn-unused:_",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen"
)

lazy val mess = project
  .in(file("."))
  .settings(publishSettings)
  .settings(noPublishSettings)
  .settings(Compile / console / scalacOptions --= warnCompilerOptions)
  .settings(Compile / console / scalacOptions += "-Yrepl-class-based")
  .settings(ThisBuild / Test / fork := true)
  .aggregate(core, benchmark, examples)
  .dependsOn(core, benchmark, examples)

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  homepage := Some(url("https://github.com/tkrs/mess")),
  licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := (_ => false),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots".at(nexus + "content/repositories/snapshots"))
    else
      Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
  },
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/tkrs/mess"),
      "scm:git:git@github.com:tkrs/mess.git"
    )
  ),
  pomExtra :=
    <developers>
      <developer>
        <id>tkrs</id>
        <name>Takeru Sato</name>
        <url>https://github.com/tkrs</url>
      </developer>
    </developers>
)

lazy val noPublishSettings = Seq(
  publish / skip := true
)

lazy val crossVersionSharedSources =
  Seq(Compile, Test).map { sc =>
    (sc / unmanagedSourceDirectories) ++= {
      (sc / unmanagedSourceDirectories).value.flatMap { dir: File =>
        if (dir.getName != "scala") Seq(dir)
        else
          CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((2, n)) if n >= 13 => Seq(new File(dir.getPath + "_2.13+"))
            case _                       => Seq(new File(dir.getPath + "_2.12-"))
          }
      }
    }
  }

lazy val core = project
  .in(file("modules/core"))
  .settings(publishSettings)
  .settings(crossVersionSharedSources)
  .settings(
    description := "mess core",
    moduleName := "mess-core"
  )
  .settings(Compile / sourceGenerators += (Compile / sourceManaged).map(Boilerplate.gen).taskValue)
  .settings(
    libraryDependencies ++= Seq(
      Pkg.msgpackJava,
      Pkg.shapeless
    )
  )

lazy val examples = (project in file("modules/examples"))
  .settings(publishSettings)
  .settings(noPublishSettings)
  .settings(
    description := "mess examples",
    moduleName := "mess-examples"
  )
  .settings(
    coverageEnabled := false
  )
  .dependsOn(core)

lazy val benchmark = (project in file("modules/benchmark"))
  .settings(publishSettings)
  .settings(noPublishSettings)
  .settings(
    description := "mess benchmark",
    moduleName := "mess-benchmark"
  )
  .settings(
    coverageEnabled := false
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core % "test->test")
