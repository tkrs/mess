import sbt._

object Dependencies {
  val Ver = new {
    val `scala2.13` = "2.13.3"
    val `scala2.12` = "2.12.12"

    val organizeImports = "0.4.0"

    val shapeless   = "2.3.3"
    val msgpackJava = "0.8.20"
    val scalatest   = "3.2.2"
    val scalacheck  = "3.2.2.0"
  }

  lazy val OrganizeImports = "com.github.liancheng" %% "organize-imports" % Ver.organizeImports

  lazy val Shapeless   = "com.chuusai"       %% "shapeless"       % Ver.shapeless
  lazy val MsgpackJava = "org.msgpack"        % "msgpack-core"    % Ver.msgpackJava
  lazy val Scalatest   = "org.scalatest"     %% "scalatest"       % Ver.scalatest
  lazy val Scalacheck  = "org.scalatestplus" %% "scalacheck-1-14" % Ver.scalacheck
}
