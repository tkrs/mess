package com.github.tkrs

import mess._
import mess.codec.Decoder
import mess.codec.Encoder
import mess.codec.semiauto._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class CodecCheckerAdt extends MsgpackHelper {

  sealed trait Z
  case class Y(int: Int, long: Long) extends Z
  object Y {
    implicit val decode: Decoder[Y] = derivedDecoder[Y]
    implicit val encode: Encoder[Y] = derivedEncoder[Y]
  }
  case class X(string: String) extends Z
  object X {
    implicit val decode: Decoder[X] = derivedDecoder[X]
    implicit val encode: Encoder[X] = derivedEncoder[X]
  }

  implicit val arbZ: Arbitrary[Z] = Arbitrary(
    Gen.oneOf(
      Arbitrary.arbitrary[String].map(X.apply),
      Arbitrary.arbitrary[(Int, Long)].map((Y.apply _).tupled)
    )
  )

  test("ADT") {
    import mess.codec.auto._
    roundTrip[Z]
  }
}
