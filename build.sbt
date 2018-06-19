import Dependencies._

lazy val root = (project in file("."))
  .settings(allSettings)
  .settings(noPublishSettings)
  .aggregate(core, benchmark)
  .dependsOn(core, benchmark)

lazy val allSettings = buildSettings ++ baseSettings ++ publishSettings

lazy val buildSettings = Seq(
  name := "mess",
  organization := "com.github.tkrs",
  scalaVersion := Ver.`scala2.12`,
  crossScalaVersions := Seq(
    Ver.`scala2.11`,
    Ver.`scala2.12`
  ),
  addCompilerPlugin(Pkg.kindProjector)
)

lazy val baseSettings = Seq(
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= Pkg.forTest,
  scalacOptions ++= compilerOptions ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, p)) if p >= 12 =>
        Seq(
          "-Ywarn-extra-implicit",
          "-Ywarn-unused:implicits",
          "-Ywarn-unused:imports",
          "-Ywarn-unused:locals",
          // "-Ywarn-unused:params",
          "-Ywarn-unused:patvars",
          "-Ywarn-unused:privates"
        )
      case _ =>
        Seq.empty
    }
  },
  scalacOptions in (Compile, console) ~= (_.filterNot(_.startsWith("-Ywarn-unused"))),
  scalacOptions in (Compile, console) ++= Seq("-Yrepl-class-based"),
  fork in Test := true
)

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  homepage := Some(url("https://github.com/tkrs/mess")),
  licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/tkrs/mess"),
      "scm:git:git@github.com:tkrs/mess.git",
    )
  ),
  pomExtra :=
    <developers>
      <developer>
        <id>tkrs</id>
        <name>Takeru Sato</name>
        <url>https://github.com/tkrs</url>
      </developer>
    </developers>,
  pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toCharArray),
  pgpSecretRing := sys.env.get("PGP_SECRET_RING").fold(pgpSecretRing.value)(file),
)

lazy val noPublishSettings = Seq(
  publish := ((): Unit),
  publishLocal := ((): Unit),
  publishArtifact := false
)

lazy val core = project.in(file("modules/core"))
  .settings(allSettings)
  .settings(
    description := "mess core",
    moduleName := "mess-core",
    name := "core"
  )
  .settings(
    libraryDependencies ++= Seq(
      Pkg.msgpackJava,
      Pkg.shapeless,
      Pkg.exportHook,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      compilerPlugin("org.scalamacros" % "paradise" % Ver.macroParadise cross CrossVersion.patch)
    )
  )

lazy val benchmark = (project in file("modules/benchmark"))
  .settings(allSettings)
  .settings(noPublishSettings)
  .settings(
    description := "mess benchmark",
    moduleName := "mess-benchmark",
    name := "benchmark"
  )
  .settings(
    coverageEnabled := false
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(core % "test->test")

lazy val compilerOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Xfuture",
  "-Xlint"
)
