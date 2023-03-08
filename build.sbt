import Dependencies._

lazy val mess = project
  .in(file("."))
  .settings(sharedSettings)
  .settings(publish / skip := true)
  .settings(
    inThisBuild(
      Seq(
        organization := "com.github.tkrs",
        homepage     := Some(url("https://github.com/tkrs/mess")),
        licenses     := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
        developers := List(
          Developer(
            "tkrs",
            "Takeru Sato",
            "type.in.type@gmail.com",
            url("https://github.com/tkrs")
          )
        ),
        scalaVersion       := Ver.`scala2.13`,
        crossScalaVersions := Seq(Ver.`scala2.12`, Ver.`scala2.13`, Ver.scala3),
        libraryDependencies ++= Seq(MunitScalacheck).map(_ % Test),
        testFrameworks += new TestFramework("munit.Framework"),
        scalafmtOnCompile := true,
        scalafixOnCompile := true,
        scalafixDependencies += OrganizeImports,
        semanticdbEnabled := true,
        semanticdbVersion := scalafixSemanticdb.revision,
        fork              := true
      )
    )
  )
  .settings(Compile / console / scalacOptions --= warnCompilerOptions)
  .settings(Compile / console / scalacOptions += "-Yrepl-class-based")
  .aggregate(core, benchmark, examples)
  .dependsOn(core, benchmark, examples)

lazy val core = project
  .in(file("modules/core"))
  .settings(sharedSettings)
  .settings(crossVersionSharedSources)
  .settings(
    description := "mess core",
    moduleName  := "mess-core"
  )
  .settings(Compile / sourceGenerators += (Compile / sourceManaged).map(Boilerplate.gen).taskValue)
  .settings(
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => Seq(MsgpackJava)
        case _            => Seq(MsgpackJava, Shapeless)
      }
    }
  )

lazy val examples = project
  .in(file("modules/examples"))
  .settings(sharedSettings)
  .settings(publish / skip := true)
  .settings(
    description := "mess examples",
    moduleName  := "mess-examples"
  )
  .settings(
    coverageEnabled := false
  )
  .dependsOn(core)

lazy val benchmark = project
  .in(file("modules/benchmark"))
  .settings(sharedSettings)
  .settings(publish / skip := true)
  .settings(
    description := "mess benchmark",
    moduleName  := "mess-benchmark"
  )
  .settings(
    coverageEnabled := false
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core % "test->test")

lazy val sharedSettings = Seq(
  scalacOptions ++= compilerOptions ++ warnCompilerOptions ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Nil // Seq("-source:3.0-migration")
      case Some((2, n)) if n >= 13 =>
        Seq("-Xfatal-warnings",
            "-Xlint",
            "-Ywarn-unused",
            "-Ywarn-extra-implicit",
            "-Ywarn-dead-code",
            "-Ywarn-numeric-widen"
        )

      case _ => Seq("-Xfuture", "-Ypartial-unification", "-Yno-adapted-args")
    }
  }
)

lazy val crossVersionSharedSources =
  Seq(Compile, Test).map { sc =>
    (sc / unmanagedSourceDirectories) ++= {
      (sc / unmanagedSourceDirectories).value.flatMap {
        dir: File =>
          if (dir.getName != "scala") Seq(dir)
          else
            CrossVersion.partialVersion(scalaVersion.value) match {
              case Some((3, n)) =>
                Seq(new File(dir.getPath + "_3"))
              case Some((2, n)) if n >= 13 =>
                Seq(new File(dir.getPath + "_2"), new File(dir.getPath + "_2.13+"))
              case s =>
                Seq(new File(dir.getPath + "_2"), new File(dir.getPath + "_2.12-"))
            }
      }
    }
  }

lazy val compilerOptions = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-unchecked",
  "-language:higherKinds",
  "-feature"
)

lazy val warnCompilerOptions = Seq(
  "-Xfatal-warnings"
)
