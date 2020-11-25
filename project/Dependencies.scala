import sbt._

object Dependencies {
  val Ver = new {
    val `scala2.13` = "2.13.3"
    val `scala2.12` = "2.12.12"

    val organizeImports = "0.4.4"

    val shapeless   = "2.3.3"
    val msgpackJava = "0.8.21"

    val munit = "0.7.19"
  }

  lazy val OrganizeImports = "com.github.liancheng" %% "organize-imports" % Ver.organizeImports

  lazy val Shapeless       = "com.chuusai"   %% "shapeless"        % Ver.shapeless
  lazy val MsgpackJava     = "org.msgpack"    % "msgpack-core"     % Ver.msgpackJava
  lazy val MunitScalacheck = "org.scalameta" %% "munit-scalacheck" % Ver.munit
}
