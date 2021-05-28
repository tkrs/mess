import sbt._

object Dependencies {
  val Ver = new {
    val `scala2.13` = "2.13.5"
    val `scala2.12` = "2.12.14"

    val organizeImports = "0.5.0"

    val shapeless   = "2.3.7"
    val msgpackJava = "0.8.24"

    val munit = "0.7.26"
  }

  lazy val OrganizeImports = "com.github.liancheng" %% "organize-imports" % Ver.organizeImports

  lazy val Shapeless       = "com.chuusai"   %% "shapeless"        % Ver.shapeless
  lazy val MsgpackJava     = "org.msgpack"    % "msgpack-core"     % Ver.msgpackJava
  lazy val MunitScalacheck = "org.scalameta" %% "munit-scalacheck" % Ver.munit
}
