package mess.codec

import mess.{Fmt, MsgpackHelper}
import org.msgpack.core.MessagePack
import org.scalatest.funsuite.AnyFunSuite

class EncoderSpec extends AnyFunSuite with MsgpackHelper {
  case class Bar(double: Double)

  object Bar {
    implicit val encode: Encoder[Bar] = semiauto.derivedEncoder[Bar]
  }
  case class Foo(int: Int, str: String, bar: Bar)

  object Foo {
    implicit val encode: Encoder[Foo] = semiauto.derivedEncoder[Foo]
  }

  case class Qux(byte: Option[Int])

  object Qux {
    implicit val encode: Encoder[Qux] = semiauto.derivedEncoder[Qux]
  }

  def check[A](tc: Seq[(A, Fmt)])(implicit encodeA: Encoder[A]): Unit =
    for ((p, expected) <- tc) {
      packer.clear()
      encodeA(p).pack(packer)
      val m = Fmt.unpack(MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(packer.toByteArray))
      packer.clear()
      assert(m === expected)
    }

  test("Encoder[Some[A]]") {
    check {
      Seq((Some(1), Fmt.fromByte(1.toByte)))
    }
  }

  test("Encoder[Char]") {
    check {
      Seq(('a', Fmt.fromString("a")))
    }
  }

  test("Encoder[None.type]") {
    check {
      Seq((None, Fmt.nil))
    }
  }

  test("Encoder[Option[A]]") {
    check {
      Seq(
        (Option(Int.MaxValue), Fmt.fromInt(Int.MaxValue)),
        (Option.empty[Int], Fmt.nil)
      )
    }
  }

  test("Encoder[Boolean]") {
    check {
      Seq(
        (true, Fmt.fromBoolean(true)),
        (false, Fmt.fromBoolean(false))
      )
    }
  }

  test("Encoder[Byte]") {
    check {
      Seq(
        (0.toByte, Fmt.fromByte(0.toByte)),
        (-32.toByte, Fmt.fromByte(-32.toByte)),
        (-33.toByte, Fmt.fromByte(-33.toByte)),
        (Byte.MaxValue, Fmt.fromByte(Byte.MaxValue)),
        (Byte.MinValue, Fmt.fromByte(Byte.MinValue))
      )
    }
  }

  test("Encoder[Short]") {
    check {
      Seq(
        (Byte.MaxValue.toShort, Fmt.fromByte(Byte.MaxValue)),
        (Byte.MinValue.toShort, Fmt.fromByte(Byte.MinValue)),
        ((Byte.MaxValue.toShort + 1.toShort).toShort, Fmt.fromShort((Byte.MaxValue.toShort + 1.toShort).toShort)),
        ((Byte.MinValue.toShort - 1.toShort).toShort, Fmt.fromShort((Byte.MinValue.toShort - 1.toShort).toShort)),
        (255.toShort, Fmt.fromShort(255.toShort)),
        (256.toShort, Fmt.fromShort(256.toShort)),
        (Short.MaxValue, Fmt.fromShort(Short.MaxValue)),
        (Short.MinValue, Fmt.fromShort(Short.MinValue))
      )
    }
  }

  test("Encoder[Int]") {
    check {
      Seq(
        (Short.MaxValue.toInt, Fmt.fromShort(Short.MaxValue)),
        (Short.MinValue.toInt, Fmt.fromShort(Short.MinValue)),
        (Short.MaxValue.toInt + 1, Fmt.fromInt(Short.MaxValue.toInt + 1)),
        (Short.MinValue.toInt - 1, Fmt.fromInt(Short.MinValue.toInt - 1)),
        (65535, Fmt.fromInt(65535)),
        (65536, Fmt.fromInt(65536)),
        (Int.MaxValue, Fmt.fromInt(Int.MaxValue)),
        (Int.MinValue, Fmt.fromInt(Int.MinValue))
      )
    }
  }

  test("Encoder[Long]") {
    check {
      Seq(
        (Int.MaxValue.toLong, Fmt.fromInt(Int.MaxValue)),
        (Int.MinValue.toLong, Fmt.fromInt(Int.MinValue)),
        (Int.MinValue - 1L, Fmt.fromLong(Int.MinValue - 1L)),
        (Int.MaxValue + 1L, Fmt.fromLong(Int.MaxValue + 1L)),
        (Long.MaxValue, Fmt.fromLong(Long.MaxValue)),
        (Long.MinValue, Fmt.fromLong(Long.MinValue))
      )
    }
  }

  test("Encoder[BigInt]") {
    check {
      Seq(
        (BigInt(Long.MaxValue), Fmt.fromLong(Long.MaxValue)),
        (BigInt(Long.MinValue), Fmt.fromLong(Long.MinValue)),
        ((BigInt(1) << 64) - 1, Fmt.fromBigInt((BigInt(1) << 64) - 1))
      )
    }
  }

  test("Encoder[Double]") {
    check {
      Seq(
        (0.0, Fmt.fromDouble(0.0)),
        (Double.MaxValue, Fmt.fromDouble(Double.MaxValue)),
        (Double.MinValue, Fmt.fromDouble(Double.MinValue))
      )
    }
  }

  test("Encoder[Float]") {
    check {
      Seq(
        (0.0f, Fmt.MFloat(0.0f)),
        (Float.MaxValue, Fmt.MFloat(Float.MaxValue)),
        (Float.MinValue, Fmt.MFloat(Float.MinValue))
      )
    }
  }

  test("Encoder[Seq[A]]") {
    check {
      Seq(
        (Seq(0 to 14: _*), Fmt.fromValues((0 to 14).map(a => Fmt.fromByte(a.toByte)): _*)),
        (Seq.empty[Int], Fmt.fromValues())
      )
    }
  }

  test("Encoder[List[A]]") {
    check {
      Seq(
        ((0 to 14).toList, Fmt.fromValues((0 to 14).map(a => Fmt.fromByte(a.toByte)): _*))
      )
    }
  }

  test("Encoder[Vector[A]]") {
    check {
      Seq(
        ((0 to 14).toVector, Fmt.fromValues((0 to 14).map(a => Fmt.fromByte(a.toByte)): _*))
      )
    }
  }

  test("Encoder[Map[A, B]]") {
    check {
      Seq(
        (
          ('a' to 'z').zip(0 to 14).toMap,
          Fmt.fromEntries(('a' to 'z').zip(0 to 14).map {
            case (k, v) => Fmt.fromString(k.toString) -> Fmt.fromByte(v.toByte)
          }: _*)
        ),
        (Map.empty[Char, Int], Fmt.fromEntries())
      )
    }
  }

  test("Encoder[Map[A, Bar]]") {
    check {
      Seq(
        (
          ('a' to 'z').zip((0 to 14).map(a => Bar(a.toDouble))).toMap,
          Fmt.fromEntries(('a' to 'z').zip(0 to 14).map {
            case (k, v) =>
              Fmt.fromString(k.toString) -> Fmt
                .fromEntries(Fmt.fromString("double") -> Fmt.fromDouble(v.toDouble))
          }: _*)
        )
      )
    }
  }

  test("Encoder[Qux]") {
    check {
      Seq(
        (Qux(None), Fmt.fromEntries(Fmt.fromString("byte") -> Fmt.nil)),
        (Qux(Some(1)), Fmt.fromEntries(Fmt.fromString("byte") -> Fmt.fromByte(1.toByte)))
      )
    }
  }

  test("contramap") {
    val encode = Encoder[Int].contramap[String](_.toInt)
    assert(encode("10") == Fmt.MInt(10))
  }

  test("map") {
    val encode = Encoder[Map[String, Option[Int]]].map {
      case m: Fmt.MMap => Fmt.MMap(m.value.filterNot(_._2 == Fmt.MNil))
      case _           => fail()
    }
    assert(
      encode(Map("a" -> Some(10), "b" -> None)) == Fmt.fromEntries(Fmt.fromString("a") -> Fmt.fromInt(10))
    )
  }
}
