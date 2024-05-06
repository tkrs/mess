import sbt._

object Dependencies {
  val Ver = new {
    val scala3      = "3.4.1"
    val `scala2.13` = "2.13.14"
    val `scala2.12` = "2.12.19"

    val organizeImports = "0.6.0"

    val shapeless   = "2.3.10"
    val msgpackJava = "0.9.8"

    val munit = "0.7.29"
  }

  lazy val OrganizeImports = "com.github.liancheng" %% "organize-imports" % Ver.organizeImports

  lazy val Shapeless       = "com.chuusai"   %% "shapeless"        % Ver.shapeless
  lazy val MsgpackJava     = "org.msgpack"    % "msgpack-core"     % Ver.msgpackJava
  lazy val MunitScalacheck = "org.scalameta" %% "munit-scalacheck" % Ver.munit
}
