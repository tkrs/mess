import sbt._

object Dependencies {
  val Ver = new {
    val `scala2.13` = "2.13.1"
    val `scala2.12` = "2.12.10"
    val shapeless   = "2.3.3"
    val scalacheck  = "1.14.0"
    val scalatest   = "3.0.8"
    val msgpackJava = "0.8.17"
  }

  val Pkg = new {
    lazy val shapeless   = "com.chuusai"    %% "shapeless"   % Ver.shapeless
    lazy val scalatest   = "org.scalatest"  %% "scalatest"   % Ver.scalatest
    lazy val scalacheck  = "org.scalacheck" %% "scalacheck"  % Ver.scalacheck
    lazy val msgpackJava = "org.msgpack"    % "msgpack-core" % Ver.msgpackJava

    lazy val forTest = Seq(scalatest, scalacheck).map(_ % Test)
  }
}
