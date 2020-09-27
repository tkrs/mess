package mess.codec

import mess.{Fmt, MsgpackHelper}
import org.msgpack.core.MessagePack

class EncoderSpec extends MsgpackHelper {
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

  private def check[A](name: String, tc: Seq[(A, Fmt)])(implicit encodeA: Encoder[A]) =
    test(name) {
      for ((p, expected) <- tc) {
        packer.clear()
        encodeA(p).pack(packer)
        val m = Fmt.unpack(MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(packer.toByteArray))
        packer.clear()
        assertEquals(m, expected)
      }
    }

  check("Encoder[Some[A]]", Seq((Some(1), Fmt.fromByte(1.toByte))))
  check("Encoder[Char]", Seq(('a', Fmt.fromString("a"))))
  check("Encoder[None.type]", Seq((None, Fmt.nil)))
  check("Encoder[Option[A]]",
        Seq(
          (Option(Int.MaxValue), Fmt.fromInt(Int.MaxValue)),
          (Option.empty[Int], Fmt.nil)
        )
  )
  check("Encoder[Boolean]",
        Seq(
          (true, Fmt.fromBoolean(true)),
          (false, Fmt.fromBoolean(false))
        )
  )
  check(
    "Encoder[Byte]",
    Seq(
      (0.toByte, Fmt.fromByte(0.toByte)),
      (-32.toByte, Fmt.fromByte(-32.toByte)),
      (-33.toByte, Fmt.fromByte(-33.toByte)),
      (Byte.MaxValue, Fmt.fromByte(Byte.MaxValue)),
      (Byte.MinValue, Fmt.fromByte(Byte.MinValue))
    )
  )
  check(
    "Encoder[Short]",
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
  )
  check(
    "Encoder[Int]",
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
  )
  check(
    "Encoder[Long]",
    Seq(
      (Int.MaxValue.toLong, Fmt.fromInt(Int.MaxValue)),
      (Int.MinValue.toLong, Fmt.fromInt(Int.MinValue)),
      (Int.MinValue - 1L, Fmt.fromLong(Int.MinValue - 1L)),
      (Int.MaxValue + 1L, Fmt.fromLong(Int.MaxValue + 1L)),
      (Long.MaxValue, Fmt.fromLong(Long.MaxValue)),
      (Long.MinValue, Fmt.fromLong(Long.MinValue))
    )
  )
  check(
    "Encoder[BigInt]",
    Seq(
      (BigInt(Long.MaxValue), Fmt.fromLong(Long.MaxValue)),
      (BigInt(Long.MinValue), Fmt.fromLong(Long.MinValue)),
      ((BigInt(1) << 64) - 1, Fmt.fromBigInt((BigInt(1) << 64) - 1))
    )
  )
  check("Encoder[Double]",
        Seq(
          (0.0, Fmt.fromDouble(0.0)),
          (Double.MaxValue, Fmt.fromDouble(Double.MaxValue)),
          (Double.MinValue, Fmt.fromDouble(Double.MinValue))
        )
  )
  check("Encoder[Float]",
        Seq(
          (0.0f, Fmt.MFloat(0.0f)),
          (Float.MaxValue, Fmt.MFloat(Float.MaxValue)),
          (Float.MinValue, Fmt.MFloat(Float.MinValue))
        )
  )
  check("Encoder[Seq[A]]",
        Seq(
          (Seq(0 to 14: _*), Fmt.fromValues((0 to 14).map(a => Fmt.fromByte(a.toByte)): _*)),
          (Seq.empty[Int], Fmt.fromValues())
        )
  )
  check("Encoder[List[A]]",
        Seq(
          ((0 to 14).toList, Fmt.fromValues((0 to 14).map(a => Fmt.fromByte(a.toByte)): _*))
        )
  )
  check("Encoder[Vector[A]]",
        Seq(
          ((0 to 14).toVector, Fmt.fromValues((0 to 14).map(a => Fmt.fromByte(a.toByte)): _*))
        )
  )
  check(
    "Encoder[Map[A, B]]",
    Seq(
      (
        ('a' to 'z').zip(0 to 14).toMap,
        Fmt.fromEntries(('a' to 'z').zip(0 to 14).map { case (k, v) =>
          Fmt.fromString(k.toString) -> Fmt.fromByte(v.toByte)
        }: _*)
      ),
      (Map.empty[Char, Int], Fmt.fromEntries())
    )
  )
  check(
    "Encoder[Map[A, Bar]]",
    Seq(
      (
        ('a' to 'z').zip((0 to 14).map(a => Bar(a.toDouble))).toMap,
        Fmt.fromEntries(('a' to 'z').zip(0 to 14).map { case (k, v) =>
          Fmt.fromString(k.toString) -> Fmt
            .fromEntries(Fmt.fromString("double") -> Fmt.fromDouble(v.toDouble))
        }: _*)
      )
    )
  )
  check(
    "Encoder[Qux]",
    Seq(
      (Qux(None), Fmt.fromEntries(Fmt.fromString("byte") -> Fmt.nil)),
      (Qux(Some(1)), Fmt.fromEntries(Fmt.fromString("byte") -> Fmt.fromByte(1.toByte)))
    )
  )

  test("contramap") {
    val encode = Encoder[Int].contramap[String](_.toInt)
    assert(encode("10") == Fmt.MInt(10))
  }

  test("map") {
    val encode = Encoder[Map[String, Option[Int]]].map {
      case m: Fmt.MMap => Fmt.MMap(m.value.filterNot(_._2 == Fmt.MNil))
      case _           => Fmt.nil
    }
    assert(
      encode(Map("a" -> Some(10), "b" -> None)) == Fmt.fromEntries(Fmt.fromString("a") -> Fmt.fromInt(10))
    )
  }
}
