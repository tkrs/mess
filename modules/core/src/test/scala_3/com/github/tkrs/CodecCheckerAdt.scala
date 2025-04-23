package com.github.tkrs

import mess._
import mess.codec.Decoder
import mess.codec.Encoder
import mess.codec.semiauto._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class CodecCheckerAdt extends MsgpackHelper:

  sealed trait Z derives Decoder, Encoder.AsMap
  object Z:
    final case class Y(int: Int, long: Long) extends Z
    final case class X(string: String)       extends Z

  given Arbitrary[Z] = Arbitrary(
    Gen.oneOf(
      Arbitrary.arbitrary[String].map(Z.X.apply),
      Arbitrary.arbitrary[(Int, Long)].map(Z.Y.apply.tupled)
    )
  )

  test("ADT"):
    roundTrip[Z]

  enum K derives Decoder, Encoder.AsMap:
    case Y(int: Int, long: Long)
    case X(string: String)

  given Arbitrary[K] = Arbitrary(
    Gen.oneOf(
      Arbitrary.arbitrary[String].map(K.X.apply),
      Arbitrary.arbitrary[(Int, Long)].map(K.Y.apply.tupled)
    )
  )

  test("Enum"):
    roundTrip[Z]
