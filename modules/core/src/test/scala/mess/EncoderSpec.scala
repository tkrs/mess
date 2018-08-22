package mess

import mess.ast.MsgPack
import mess.codec.Codec
import org.scalatest.FunSuite
import mess.codec.generic.derived._
import org.msgpack.core.MessagePack

class EncoderSpec extends FunSuite with MsgpackHelper {

  case class Bar(double: Double)
  case class Foo(int: Int, str: String, bar: Bar)

  case class Qux(byte: Option[Int])

  def check[A](tc: Seq[A])(implicit encodeA: Encoder[A], decodeA: Decoder[A]): Unit = {
    for (p <- tc) {
      packer.clear()
      Codec.serialize(encodeA(p), packer)
      val m = Codec.deserialize(MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(packer.toByteArray))
      val a = decodeA(m).right.get
      assert(a === p)
    }
  }

  test("Encoder[Some[Int]]") {
    check {
      Seq(Some(1))
    }
  }

  test("Encoder[Char]") {
    check {
      Seq('a')
    }
  }

  test("Encoder[None.type]") {
    check {
      Seq(None)
    }
  }

  test("Encoder[Option[Int]]") {
    check {
      Seq(
        Option(1),
        Option.empty[Int]
      )
    }
  }

  test("Encoder[Boolean]") {
    check {
      Seq(
        true,
        false
      )
    }
  }

  test("Encoder[Byte]") {
    check {
      Seq(
        0.toByte,
        (-32).toByte,
        (-33).toByte,
        Byte.MaxValue,
        Byte.MinValue
      )
    }
  }

  test("Encoder[Short]") {
    check {
      Seq(
        Byte.MaxValue.toShort,
        Byte.MinValue.toShort,
        (Byte.MaxValue.toShort + 1.toShort).toShort,
        (Byte.MinValue.toShort - 1.toShort).toShort,
        255.toShort,
        256.toShort,
        Short.MaxValue,
        Short.MinValue
      )
    }
  }

  test("Encoder[Int]") {
    check {
      Seq(
        Short.MaxValue.toInt,
        Short.MinValue.toInt,
        Short.MaxValue.toInt + 1,
        65535,
        65536,
        Int.MaxValue,
        Int.MinValue
      )
    }
  }

  test("Encoder[Long]") {
    check {
      Seq(
        Int.MaxValue.toLong,
        Int.MinValue.toLong,
        Int.MinValue - 1L,
        Int.MaxValue + 1L,
        Long.MaxValue,
        Long.MinValue
      )
    }
  }

  test("Encoder[BigInt]") {
    check {
      Seq(
        BigInt(Long.MaxValue),
        BigInt(Long.MinValue),
        (BigInt(1) << 64) - 1
      )
    }
  }

  test("Encoder[Double]") {
    check {
      Seq(
        0.0,
        Double.MaxValue,
        Double.MinValue
      )
    }
  }

  test("Encoder[Float]") {
    check {
      Seq(
        0.0f,
        Float.MaxValue,
        Float.MinValue
      )
    }
  }

  test("Encoder[Seq[A]]") {
    check {
      Seq(
        Seq(0 to 14: _*)
      )
    }
  }

  test("Encoder[List[A]]") {
    check {
      Seq(
        (0 to 14).toList
      )
    }
  }

  test("Encoder[Vector[A]]") {
    check {
      Seq(
        (0 to 14).toVector
      )
    }
  }

  test("Encoder[Map[A, B]]") {
    check {
      Seq(
        ('a' to 'z').zip(0 to 14).toMap
      )
    }
  }

  test("Encoder[Map[A, Bar]]") {
    check {
      Seq(
        ('a' to 'z').zip((0 to 14).map(a => Bar(a.toDouble))).toMap
      )
    }
  }

  test("Encoder[Qux]") {
    check {
      Seq(
        Qux(None),
        Qux(Some(1))
      )
    }
  }

  test("contramap") {
    val encode = Encoder[Int].contramap[String](_.toInt)
    assert(encode("10") == MsgPack.MInt(10))
  }
}
