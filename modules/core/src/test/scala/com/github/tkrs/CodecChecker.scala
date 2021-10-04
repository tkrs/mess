package com.github.tkrs

import java.time.Instant

import mess._
import mess.codec.Decoder
import mess.codec.Encoder
import mess.codec.semiauto._
import org.msgpack.core.MessagePack
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Shrink

class CodecChecker extends MsgpackHelper {
  import MsgpackHelper._

  implicit val arbBigInt: Arbitrary[BigInt] = Arbitrary(gen.genBigInt)

  case class User[F[_]](int: Int, friends: F[User[F]])

  object User {
    implicit val encode: Encoder[User[List]] = derivedEncoder[User[List]]
    implicit val decode: Decoder[User[List]] = derivedDecoder[User[List]]

    private def fix(depth: Int, i: Int, acc: User[List]): User[List] =
      if (depth == 0) acc
      else fix(depth - 1, i + 1, User(i, List(acc)))

    private val genFix: Gen[User[List]] = for {
      depth <- Gen.chooseNum(1, 100)
      i     <- Gen.size
    } yield fix(depth, i, User[List](i, Nil))

    implicit val arbFix: Arbitrary[User[List]] = Arbitrary(genFix)
  }

  implicit val arbInstant: Arbitrary[Instant] = Arbitrary(for {
    seconds <- Gen.choose(0L, System.currentTimeMillis() / 1000)
    nanos   <- Gen.choose(0L, 999000000L)
  } yield Instant.ofEpochSecond(seconds, nanos))

  def roundTrip[A: Arbitrary: Shrink](implicit encode: Encoder[A], decode: Decoder[A]) =
    Prop.forAll { (a: A) =>
      val ast = encode(a)
      ast.pack(packer)
      val bytes    = packer.toByteArray
      val unpacker = MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(bytes)
      val actual   = decode(Fmt.unpack(unpacker)).toTry.get
      packer.clear()
      actual == a
    }

  // format: off
  test("Boolean")(roundTrip[Boolean])
  test("Array[Byte]")(roundTrip[Array[Byte]])
  test("Byte")(roundTrip[Byte])
  test("Short")(roundTrip[Short])
  test("Int")(roundTrip[Int])
  test("Long")(roundTrip[Long])
  test("Float")(roundTrip[Float])
  test("Double")(roundTrip[Double])
  test("String")(roundTrip[String])
  test("BigInt")(roundTrip[BigInt])
  test("Timestamp")(roundTrip[Instant])
  test("Map[String, Int]")(roundTrip[Map[String, Int]])
  test("Map[String, Long]")(roundTrip[Map[String, Long]])
  test("Map[String, Float]")(roundTrip[Map[String, Float]])
  test("Map[String, Double]")(roundTrip[Map[String, Double]])
  test("Map[String, BigInt]")(roundTrip[Map[String, BigInt]])
  test("Map[String, String]")(roundTrip[Map[String, String]])
  test("Map[Int, BigInt]")(roundTrip[Map[Int, BigInt]])
  test("Vector[Int]")(roundTrip[Vector[Int]])
  test("Vector[Long]")(roundTrip[Vector[Long]])
  test("Vector[Float]")(roundTrip[Vector[Float]])
  test("Vector[Double]")(roundTrip[Vector[Double]])
  test("Vector[BigInt]")(roundTrip[Vector[BigInt]])
  test("Vector[String]")(roundTrip[Vector[String]])
  test("List[Int]")(roundTrip[List[Int]])
  test("List[Long]")(roundTrip[List[Long]])
  test("List[Float]")(roundTrip[List[Float]])
  test("List[Double]")(roundTrip[List[Double]])
  test("List[BigInt]")(roundTrip[List[BigInt]])
  test("List[String]")(roundTrip[List[String]])
  test("Seq[Int]")(roundTrip[Seq[Int]])
  test("Seq[Long]")(roundTrip[Seq[Long]])
  test("Seq[Float]")(roundTrip[Seq[Float]])
  test("Seq[Double]")(roundTrip[Seq[Double]])
  test("Seq[BigInt]")(roundTrip[Seq[BigInt]])
  test("Seq[String]")(roundTrip[Seq[String]])
  test("Set[Int]")(roundTrip[Set[Int]])
  test("Set[Long]")(roundTrip[Set[Long]])
  test("Set[Float]")(roundTrip[Set[Float]])
  test("Set[Double]")(roundTrip[Set[Double]])
  test("Set[BigInt]")(roundTrip[Set[BigInt]])
  test("Set[String]")(roundTrip[Set[String]])
  test("User[List]")(roundTrip[User[List]])
  test("Tuple1")(roundTrip[Tuple1[Int]])
  test("Tuple2")(roundTrip[Tuple2[Int, Int]])
  test("Tuple3")(roundTrip[Tuple3[Int, Int, Long]])
  test("Tuple4")(roundTrip[Tuple4[Int, Int, Long, String]])
  test("Tuple5")(roundTrip[Tuple5[Int, Int, Long, String, BigInt]])
  test("Tuple6")(roundTrip[Tuple6[Int, Int, Long, String, BigInt, Double]])
  test("Tuple7")(roundTrip[Tuple7[Int, Int, Long, String, BigInt, Double, Float]])
  test("Tuple8")(roundTrip[Tuple8[Int, Int, Long, String, BigInt, Double, Float, Long]])
  test("Tuple9")(roundTrip[Tuple9[Int, Int, Long, String, BigInt, Double, Float, Long, Int]])
  test("Tuple10")(roundTrip[Tuple10[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte]])
  test("Tuple11")(roundTrip[Tuple11[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short]])
  test("Tuple12")(roundTrip[Tuple12[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short, Boolean]])
  test("Tuple13")(roundTrip[Tuple13[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short, Boolean, Int]])
  test("Tuple14")(roundTrip[Tuple14[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short, Boolean, Int, String]])
  test("Tuple15")(roundTrip[Tuple15[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short, Boolean, Int, String, Int]])
  test("Tuple16")(roundTrip[Tuple16[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short, Boolean, Int, String, Int, Int]])
  test("Tuple17")(roundTrip[Tuple17[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short, Boolean, Int, String, Int, Int, Int]])
  test("Tuple18")(roundTrip[Tuple18[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short, Boolean, Int, String, Int, Int, Int, Int]])
  test("Tuple19")(roundTrip[Tuple19[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short, Boolean, Int, String, Int, Int, Int, Int, Int]])
  test("Tuple20")(roundTrip[Tuple20[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short, Boolean, Int, String, Int, Int, Int, Int, Int, Int]])
  test("Tuple21")(roundTrip[Tuple21[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short, Boolean, Int, String, Int, Int, Int, Int, Int, Int, Int]])
  test("Tuple22")(roundTrip[Tuple22[Int, Int, Long, String, BigInt, Double, Float, Long, Int, Byte, Short, Boolean, Int, String, List[String], Int, Int, Int, Int, Int, Int, Int]])
  // format: on

  case class Hoge(a: Option[Int])

  object Hoge {
    import mess.codec.semiauto._

    implicit val encode: Encoder[Hoge] = derivedEncoder[Hoge]
    implicit val decode: Decoder[Hoge] = derivedDecoder[Hoge]
  }

  test("null should be converted to the empty value of the corresponding data types") {
    locally {
      val in       = x"81 a1 61 c0"
      val unpacker = MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(in)
      val formated = Fmt.unpack(unpacker)
      val value    = Decoder[Hoge].apply(formated)
      assertEquals(value, Right(Hoge(None)))
    }

    val in = x"c0"

    locally {
      val unpacker = MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(in)
      val value    = Decoder[Option[Map[String, String]]].apply(Fmt.unpack(unpacker))
      assertEquals(value, Right(None))
    }

    locally {
      val unpacker = MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(in)
      val value    = Decoder[Map[String, String]].apply(Fmt.unpack(unpacker))
      assertEquals[Any, Any](value, Right(Map.empty))
    }

    locally {
      val unpacker = MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(in)
      val value    = Decoder[Vector[String]].apply(Fmt.unpack(unpacker))
      assertEquals(value, Right(Vector.empty))
    }
  }

  test("An empty array should be converted to the empty value of the corresponding data types") {
    val in = Array.emptyByteArray

    locally {
      val unpacker = MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(in)
      val value    = Decoder[Vector[String]].apply(Fmt.unpack(unpacker))
      assertEquals(value, Right(Vector.empty))
    }

    locally {
      val unpacker = MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(in)
      val value    = Decoder[Map[String, String]].apply(Fmt.unpack(unpacker))
      assertEquals[Any, Any](value, Right(Map.empty))
    }

    locally {
      val unpacker = MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(in)
      val value    = Decoder[Option[Hoge]].apply(Fmt.unpack(unpacker))
      assertEquals(value, Right(None))
    }
  }

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
