import sbt._

object Dependencies {
  val Ver = new {
    val `scala2.13` = "2.13.2"
    val `scala2.12` = "2.12.11"

    val shapeless     = "2.3.3"
    val msgpackJava   = "0.8.20"
    val scalatest     = "3.1.0"
    val scalatestplus = "3.1.0.1"
  }

  val Pkg = new {
    lazy val shapeless   = "com.chuusai"       %% "shapeless"       % Ver.shapeless
    lazy val msgpackJava = "org.msgpack"       % "msgpack-core"     % Ver.msgpackJava
    lazy val scalatest   = "org.scalatest"     %% "scalatest"       % Ver.scalatest
    lazy val scalacheck  = "org.scalatestplus" %% "scalacheck-1-14" % Ver.scalatestplus
  }
}
