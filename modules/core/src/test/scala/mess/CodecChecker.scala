package mess

import java.time.Instant

import mess.ast.MsgPack
import mess.derived.derivedCodecs._
import org.msgpack.core.MessagePack
import org.msgpack.core.MessagePack.Code
import org.scalacheck.{Arbitrary, Gen, Prop, Shrink}
import org.scalatest.prop.Checkers
import org.scalatest.{Assertion, FunSuite}

class CodecChecker extends FunSuite with Checkers with MsgpackHelper {

  implicit val arbBigInt: Arbitrary[BigInt] = Arbitrary(gen.genBigInt)

  case class User[F[_]](int: Int, friends: F[User[F]])
  object User {

    def fix(depth: Int, i: Int, acc: User[List]): User[List] = {
      if (depth == 0) acc
      else fix(depth - 1, i + 1, User(i, List(acc)))
    }

    val genFix: Gen[User[List]] = for {
      depth <- Gen.chooseNum(1, 100)
      i     <- Gen.size
    } yield fix(depth, i, User[List](i, Nil))

    implicit val arbFix: Arbitrary[User[List]] = Arbitrary(genFix)
  }

  def roundTrip[A: Encoder: Decoder: Arbitrary: Shrink]: Assertion =
    check(Prop.forAll({ a: A =>
      val encode = Encoder[A]
      val decode = Decoder[A]

      val ast = encode(a)
      Codec.serialize(ast, packer)
      val unpacker =
        MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(packer.toByteArray)
      val b = decode(Codec.deserialize(unpacker))
      packer.clear()
      a === b.right.get
    }))

  // format: off
  test("Boolean")(roundTrip[Boolean])
  test("Byte")(roundTrip[Byte])
  test("Short")(roundTrip[Short])
  test("Int")(roundTrip[Int])
  test("Long")(roundTrip[Long])
  test("Float")(roundTrip[Float])
  test("Double")(roundTrip[Double])
  test("String")(roundTrip[String])
  test("BigInt")(roundTrip[BigInt])
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

  implicit val arbInstant: Arbitrary[Instant] = Arbitrary(for {
    seconds <- Gen.choose(0L, System.currentTimeMillis() / 1000)
    nanos   <- Gen.choose(0L, 999000000L)
  } yield Instant.ofEpochSecond(seconds, nanos))

  implicit val encodeEventTime: Encoder[Instant] = new Encoder[Instant] {
    def apply(a: Instant): MsgPack = {
      val seconds = a.getEpochSecond
      val nanos   = a.getNano.toLong
      val arr = Array(
        (seconds >>> 24).toByte,
        (seconds >>> 16).toByte,
        (seconds >>> 8).toByte,
        (seconds >>> 0).toByte,
        (nanos >>> 24).toByte,
        (nanos >>> 16).toByte,
        (nanos >>> 8).toByte,
        (nanos >>> 0).toByte
      )
      MsgPack.MExtension(Code.EXT8, 8, arr)
    }
  }

  implicit val decodeEventTime: Decoder[Instant] = new Decoder[Instant] {
    def apply(a: MsgPack): Either[Throwable, Instant] = a match {
      case MsgPack.MExtension(Code.EXT8, _, arr0) =>
        val arr     = arr0.map(a => (a & 0xff).toLong)
        val seconds = (arr(0) << 24) | (arr(1) << 16) | (arr(2) << 8) | arr(3)
        val nanos   = (arr(4) << 24) | (arr(5) << 16) | (arr(6) << 8) | arr(7)
        Right(Instant.ofEpochSecond(seconds, nanos))
      case _ =>
        Left(new IllegalArgumentException(s"$a"))
    }
  }

  test("Instant(Extension)")(roundTrip[Instant])
}
