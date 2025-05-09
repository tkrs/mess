import sbt._

object Dependencies {
  val Ver = new {
    val scala3 = "3.7.0"
    val scala2 = "2.13.16"

    val shapeless   = "2.3.13"
    val msgpackJava = "0.9.9"

    val munit = "1.1.0"
  }

  lazy val Shapeless       = "com.chuusai"   %% "shapeless"        % Ver.shapeless
  lazy val MsgpackJava     = "org.msgpack"    % "msgpack-core"     % Ver.msgpackJava
  lazy val MunitScalacheck = "org.scalameta" %% "munit-scalacheck" % Ver.munit
}
