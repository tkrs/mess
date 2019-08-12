import sbt._

object Dependencies {
  val Ver = new {
    val `scala2.13`   = "2.13.0-RC1"
    val `scala2.12`   = "2.12.8"
    val shapeless     = "2.3.3"
    val scalacheck    = "1.14.0"
    val scalatest     = "3.0.7"
    val scalatestSnap = "3.0.8-RC2"
    val msgpackJava   = "0.8.16"
  }

  val Pkg = new {
    lazy val shapeless   = "com.chuusai"    %% "shapeless"   % Ver.shapeless
    lazy val scalacheck  = "org.scalacheck" %% "scalacheck"  % Ver.scalacheck
    lazy val msgpackJava = "org.msgpack"    % "msgpack-core" % Ver.msgpackJava

    def scalatest(v: String) =
      "org.scalatest" %% "scalatest" % (if (v == Ver.`scala2.13`) Ver.scalatestSnap else Ver.scalatest)
    def forTest(v: String) = Seq(scalatest(v), scalacheck).map(_ % Test)
  }
}
