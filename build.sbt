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
        scalaVersion       := Ver.scala2,
        crossScalaVersions := Seq(Ver.scala2, Ver.scala3),
        libraryDependencies ++= Seq(MunitScalacheck).map(_ % Test),
        scalafmtOnCompile := true,
        scalafixOnCompile := true,
        semanticdbEnabled := true,
        semanticdbVersion := scalafixSemanticdb.revision,
        fork              := true
      )
    )
  )
  // .settings(Compile / console / scalacOptions --= warnCompilerOptions)
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
  scalacOptions ++= compilerOptions ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq("-Wunused:imports")
      case _ =>
        Seq("-Xfatal-warnings",
            "-Xlint",
            "-Wunused:_",
            "-Ywarn-extra-implicit",
            "-Ywarn-dead-code",
            "-Ywarn-numeric-widen"
        )
    }
  }
)

lazy val crossVersionSharedSources =
  Seq(Compile, Test).map { sc =>
    (sc / unmanagedSourceDirectories) ++=
      (sc / unmanagedSourceDirectories).value.flatMap { dir: File =>
        if (dir.getName != "scala") Seq(dir)
        else
          CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((3, n)) =>
              Seq(new File(dir.getPath + "_3"))
            case _ =>
              Seq(new File(dir.getPath + "_2"), new File(dir.getPath + "_2.13+"))
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
