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
      Seq((Some(1), MsgPack.mByte(1.toByte)))
    }
  }

  test("Encoder[Char]") {
    check {
      Seq(('a', MsgPack.mStr("a")))
    }
  }

  test("Encoder[None.type]") {
    check {
      Seq((None, MsgPack.mNil))
    }
  }

  test("Encoder[Option[A]]") {
    check {
      Seq(
        (Option(Int.MaxValue), MsgPack.mInt(Int.MaxValue)),
        (Option.empty[Int], MsgPack.mNil)
      )
    }
  }

  test("Encoder[Boolean]") {
    check {
      Seq(
        (true, MsgPack.True),
        (false, MsgPack.False)
      )
    }
  }

  test("Encoder[Byte]") {
    check {
      Seq(
        (0.toByte, MsgPack.mByte(0.toByte)),
        ((-32).toByte, MsgPack.mByte((-32).toByte)),
        ((-33).toByte, MsgPack.mByte((-33).toByte)),
        (Byte.MaxValue, MsgPack.mByte(Byte.MaxValue)),
        (Byte.MinValue, MsgPack.mByte(Byte.MinValue))
      )
    }
  }

  test("Encoder[Short]") {
    check {
      Seq(
        (Byte.MaxValue.toShort, MsgPack.mByte(Byte.MaxValue)),
        (Byte.MinValue.toShort, MsgPack.mByte(Byte.MinValue)),
        ((Byte.MaxValue.toShort + 1.toShort).toShort, MsgPack.mShort((Byte.MaxValue.toShort + 1.toShort).toShort)),
        ((Byte.MinValue.toShort - 1.toShort).toShort, MsgPack.mShort((Byte.MinValue.toShort - 1.toShort).toShort)),
        (255.toShort, MsgPack.mShort(255.toShort)),
        (256.toShort, MsgPack.mShort(256.toShort)),
        (Short.MaxValue, MsgPack.mShort(Short.MaxValue)),
        (Short.MinValue, MsgPack.mShort(Short.MinValue))
      )
    }
  }

  test("Encoder[Int]") {
    check {
      Seq(
        (Short.MaxValue.toInt, MsgPack.mShort(Short.MaxValue)),
        (Short.MinValue.toInt, MsgPack.mShort(Short.MinValue)),
        (Short.MaxValue.toInt + 1, MsgPack.mInt(Short.MaxValue.toInt + 1)),
        (Short.MinValue.toInt - 1, MsgPack.mInt(Short.MinValue.toInt - 1)),
        (65535, MsgPack.mInt(65535)),
        (65536, MsgPack.mInt(65536)),
        (Int.MaxValue, MsgPack.mInt(Int.MaxValue)),
        (Int.MinValue, MsgPack.mInt(Int.MinValue))
      )
    }
  }

  test("Encoder[Long]") {
    check {
      Seq(
        (Int.MaxValue.toLong, MsgPack.mInt(Int.MaxValue)),
        (Int.MinValue.toLong, MsgPack.mInt(Int.MinValue)),
        (Int.MinValue - 1L, MsgPack.mLong(Int.MinValue - 1L)),
        (Int.MaxValue + 1L, MsgPack.mLong(Int.MaxValue + 1L)),
        (Long.MaxValue, MsgPack.mLong(Long.MaxValue)),
        (Long.MinValue, MsgPack.mLong(Long.MinValue))
      )
    }
  }

  test("Encoder[BigInt]") {
    check {
      Seq(
        (BigInt(Long.MaxValue), MsgPack.mLong(Long.MaxValue)),
        (BigInt(Long.MinValue), MsgPack.mLong(Long.MinValue)),
        ((BigInt(1) << 64) - 1, MsgPack.mBigInt((BigInt(1) << 64) - 1))
      )
    }
  }

  test("Encoder[Double]") {
    check {
      Seq(
        (0.0, MsgPack.mDouble(0.0)),
        (Double.MaxValue, MsgPack.mDouble(Double.MaxValue)),
        (Double.MinValue, MsgPack.mDouble(Double.MinValue))
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
        (Seq(0 to 14: _*), MsgPack.mArr((0 to 14).map(a => MsgPack.mByte(a.toByte)): _*)),
        (Seq.empty[Int], MsgPack.mArr())
      )
    }
  }

  test("Encoder[List[A]]") {
    check {
      Seq(
        ((0 to 14).toList, MsgPack.mArr((0 to 14).map(a => MsgPack.mByte(a.toByte)): _*))
      )
    }
  }

  test("Encoder[Vector[A]]") {
    check {
      Seq(
        ((0 to 14).toVector, MsgPack.mArr((0 to 14).map(a => MsgPack.mByte(a.toByte)): _*))
      )
    }
  }

  test("Encoder[Map[A, B]]") {
    check {
      Seq(
        (('a' to 'z').zip(0 to 14).toMap, MsgPack.mMap(('a' to 'z').zip(0 to 14).map {
          case (k, v) => MsgPack.mStr(k.toString) -> MsgPack.mByte(v.toByte)
        }: _*)),
        (Map.empty[Char, Int], MsgPack.mMap())
      )
    }
  }

  test("Encoder[Map[A, Bar]]") {
    import mess.codec.generic.derived._
    check {
      Seq(
        (('a' to 'z').zip((0 to 14).map(a => Bar(a.toDouble))).toMap, MsgPack.mMap(('a' to 'z').zip(0 to 14).map {
          case (k, v) => MsgPack.mStr(k.toString) -> MsgPack.mMap(MsgPack.mStr("double") -> MsgPack.mDouble(v.toDouble))
        }: _*))
      )
    }
  }

  test("Encoder[Qux]") {
    import mess.codec.generic.derived._
    check {
      Seq(
        (Qux(None), MsgPack.mMap(MsgPack.mStr("byte")    -> MsgPack.mNil)),
        (Qux(Some(1)), MsgPack.mMap(MsgPack.mStr("byte") -> MsgPack.mByte(1.toByte)))
      )
    }
  }

  test("contramap") {
    val encode = Encoder[Int].contramap[String](_.toInt)
    assert(encode("10") == MsgPack.MInt(10))
  }
}
