package mess

import mess.ast.MsgPack
import org.scalatest.FunSuite
import org.msgpack.core.MessagePack

class EncoderSpec extends FunSuite with MsgpackHelper {

  case class Bar(double: Double)
  case class Foo(int: Int, str: String, bar: Bar)

  case class Qux(byte: Option[Int])

  def check[A](tc: Seq[(A, MsgPack)])(implicit encodeA: Encoder[A]): Unit = {
    for ((p, expected) <- tc) {
      packer.clear()
      encodeA(p).pack(packer)
      val m = MsgPack.unpack(MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(packer.toByteArray))
      packer.clear()
      assert(m === expected)
    }
  }

  test("Encoder[Some[A]]") {
    check {
      Seq((Some(1), MsgPack.fromByte(1.toByte)))
    }
  }

  test("Encoder[Char]") {
    check {
      Seq(('a', MsgPack.fromString("a")))
    }
  }

  test("Encoder[None.type]") {
    check {
      Seq((None, MsgPack.nil))
    }
  }

  test("Encoder[Option[A]]") {
    check {
      Seq(
        (Option(Int.MaxValue), MsgPack.fromInt(Int.MaxValue)),
        (Option.empty[Int], MsgPack.nil)
      )
    }
  }

  test("Encoder[Boolean]") {
    check {
      Seq(
        (true, MsgPack.fromBoolean(true)),
        (false, MsgPack.fromBoolean(false))
      )
    }
  }

  test("Encoder[Byte]") {
    check {
      Seq(
        (0.toByte, MsgPack.fromByte(0.toByte)),
        ((-32).toByte, MsgPack.fromByte((-32).toByte)),
        ((-33).toByte, MsgPack.fromByte((-33).toByte)),
        (Byte.MaxValue, MsgPack.fromByte(Byte.MaxValue)),
        (Byte.MinValue, MsgPack.fromByte(Byte.MinValue))
      )
    }
  }

  test("Encoder[Short]") {
    check {
      Seq(
        (Byte.MaxValue.toShort, MsgPack.fromByte(Byte.MaxValue)),
        (Byte.MinValue.toShort, MsgPack.fromByte(Byte.MinValue)),
        ((Byte.MaxValue.toShort + 1.toShort).toShort, MsgPack.fromShort((Byte.MaxValue.toShort + 1.toShort).toShort)),
        ((Byte.MinValue.toShort - 1.toShort).toShort, MsgPack.fromShort((Byte.MinValue.toShort - 1.toShort).toShort)),
        (255.toShort, MsgPack.fromShort(255.toShort)),
        (256.toShort, MsgPack.fromShort(256.toShort)),
        (Short.MaxValue, MsgPack.fromShort(Short.MaxValue)),
        (Short.MinValue, MsgPack.fromShort(Short.MinValue))
      )
    }
  }

  test("Encoder[Int]") {
    check {
      Seq(
        (Short.MaxValue.toInt, MsgPack.fromShort(Short.MaxValue)),
        (Short.MinValue.toInt, MsgPack.fromShort(Short.MinValue)),
        (Short.MaxValue.toInt + 1, MsgPack.fromInt(Short.MaxValue.toInt + 1)),
        (Short.MinValue.toInt - 1, MsgPack.fromInt(Short.MinValue.toInt - 1)),
        (65535, MsgPack.fromInt(65535)),
        (65536, MsgPack.fromInt(65536)),
        (Int.MaxValue, MsgPack.fromInt(Int.MaxValue)),
        (Int.MinValue, MsgPack.fromInt(Int.MinValue))
      )
    }
  }

  test("Encoder[Long]") {
    check {
      Seq(
        (Int.MaxValue.toLong, MsgPack.fromInt(Int.MaxValue)),
        (Int.MinValue.toLong, MsgPack.fromInt(Int.MinValue)),
        (Int.MinValue - 1L, MsgPack.fromLong(Int.MinValue - 1L)),
        (Int.MaxValue + 1L, MsgPack.fromLong(Int.MaxValue + 1L)),
        (Long.MaxValue, MsgPack.fromLong(Long.MaxValue)),
        (Long.MinValue, MsgPack.fromLong(Long.MinValue))
      )
    }
  }

  test("Encoder[BigInt]") {
    check {
      Seq(
        (BigInt(Long.MaxValue), MsgPack.fromLong(Long.MaxValue)),
        (BigInt(Long.MinValue), MsgPack.fromLong(Long.MinValue)),
        ((BigInt(1) << 64) - 1, MsgPack.fromBigInt((BigInt(1) << 64) - 1))
      )
    }
  }

  test("Encoder[Double]") {
    check {
      Seq(
        (0.0, MsgPack.fromDouble(0.0)),
        (Double.MaxValue, MsgPack.fromDouble(Double.MaxValue)),
        (Double.MinValue, MsgPack.fromDouble(Double.MinValue))
      )
    }
  }

  test("Encoder[Float]") {
    check {
      Seq(
        (0.0f, MsgPack.MFloat(0.0f)),
        (Float.MaxValue, MsgPack.MFloat(Float.MaxValue)),
        (Float.MinValue, MsgPack.MFloat(Float.MinValue))
      )
    }
  }

  test("Encoder[Seq[A]]") {
    check {
      Seq(
        (Seq(0 to 14: _*), MsgPack.fromValues((0 to 14).map(a => MsgPack.fromByte(a.toByte)): _*)),
        (Seq.empty[Int], MsgPack.fromValues())
      )
    }
  }

  test("Encoder[List[A]]") {
    check {
      Seq(
        ((0 to 14).toList, MsgPack.fromValues((0 to 14).map(a => MsgPack.fromByte(a.toByte)): _*))
      )
    }
  }

  test("Encoder[Vector[A]]") {
    check {
      Seq(
        ((0 to 14).toVector, MsgPack.fromValues((0 to 14).map(a => MsgPack.fromByte(a.toByte)): _*))
      )
    }
  }

  test("Encoder[Map[A, B]]") {
    check {
      Seq(
        (('a' to 'z').zip(0 to 14).toMap, MsgPack.fromPairSeq(('a' to 'z').zip(0 to 14).map {
          case (k, v) => MsgPack.fromString(k.toString) -> MsgPack.fromByte(v.toByte)
        })),
        (Map.empty[Char, Int], MsgPack.fromPairs())
      )
    }
  }

  test("Encoder[Map[A, Bar]]") {
    import mess.codec.generic.derived._
    check {
      Seq(
        (('a' to 'z').zip((0 to 14).map(a => Bar(a.toDouble))).toMap,
         MsgPack.fromPairSeq(('a' to 'z').zip(0 to 14).map {
           case (k, v) =>
             MsgPack.fromString(k.toString) -> MsgPack.fromPairs(
               MsgPack.fromString("double") -> MsgPack.fromDouble(v.toDouble))
         }))
      )
    }
  }

  test("Encoder[Qux]") {
    import mess.codec.generic.derived._
    check {
      Seq(
        (Qux(None), MsgPack.fromPairs(MsgPack.fromString("byte")    -> MsgPack.nil)),
        (Qux(Some(1)), MsgPack.fromPairs(MsgPack.fromString("byte") -> MsgPack.fromByte(1.toByte)))
      )
    }
  }

  test("contramap") {
    val encode = Encoder[Int].contramap[String](_.toInt)
    assert(encode("10") == MsgPack.MInt(10))
  }

  test("mapMsgPack") {
    val encode = Encoder[Map[String, Option[Int]]].map {
      case MsgPack.MMap(m) => MsgPack.MMap(m.filterNot(_._2 == MsgPack.MNil))
      case _               => fail()
    }
    assert(
      encode(Map("a" -> Some(10), "b" -> None)) == MsgPack.fromPairs(MsgPack.fromString("a") -> MsgPack.fromInt(10)))
  }
}
