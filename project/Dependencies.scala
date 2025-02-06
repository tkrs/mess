import sbt._

object Dependencies {
  val Ver = new {
    val scala3      = "3.6.3"
    val `scala2.13` = "2.13.16"

    val shapeless   = "2.3.12"
    val msgpackJava = "0.9.8"

    val munit = "1.0.0"
  }

  lazy val Shapeless       = "com.chuusai"   %% "shapeless"        % Ver.shapeless
  lazy val MsgpackJava     = "org.msgpack"    % "msgpack-core"     % Ver.msgpackJava
  lazy val MunitScalacheck = "org.scalameta" %% "munit-scalacheck" % Ver.munit
}
