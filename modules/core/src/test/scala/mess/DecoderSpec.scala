package mess

import mess.ast.MsgPack
import org.scalatest.FunSuite
import mess.codec.generic.derived._

class DecoderSpec extends FunSuite with MsgpackHelper {

  case class Bar(double: Double)
  case class Foo(int: Int, str: String, bar: Bar)
  case class Qux(byte: Option[Int])

  sealed trait Z
  case class Z0(a: Int) extends Z
  case class Z1(b: Int) extends Z

  def decode[A](msg: MsgPack)(implicit A: Decoder[A]): Either[Throwable, A] = A(msg)

  def check[A: Decoder](tc: Seq[(A, MsgPack)]): Unit =
    for ((expected, p) <- tc) {
      decode(p) match {
        case Right(v) =>
          assert(v === expected)
        case Left(e) =>
          throw e
      }
    }

  test("Decoder[Some[A]]") {
    check {
      Seq(
        (Some(1), MsgPack.fromInt(1))
      )
    }
  }

  test("Decoder[Char]") {
    check {
      Seq(
        ('a', MsgPack.fromString("a"))
      )
    }
  }

  test("Decoder[Array[Byte]]") {
    check {
      Seq(
        (Array(0x11, 0x1a).map(_.toByte), MsgPack.fromBytes(Array(0x11, 0x1a).map(_.toByte)))
      )
    }
  }

  test("Decoder[Char] failure") {
    val msg = MsgPack.fromString("ab")
    assert(decode[Char](msg) === Left(TypeMismatchError("Char", msg)))
  }

  test("Decoder[None.type]") {
    check {
      Seq(
        (None, MsgPack.nil)
      )
    }
  }

  test("Decoder[Option[A]]") {
    check {
      Seq(
        (Option(1), MsgPack.fromInt(1)),
        (Option.empty[Int], MsgPack.nil)
      )
    }
  }

  test("Decoder[Boolean]") {
    check {
      Seq(
        (true, MsgPack.fromBoolean(true)),
        (false, MsgPack.fromBoolean(false))
      )
    }
  }

  test("Decoder[Boolean] failure") {
    val msg = MsgPack.empty
    assert(decode[Boolean](msg) === Left(TypeMismatchError("Boolean", msg)))
  }

  test("Decoder[Byte]") {
    check {
      Seq(
        (0.toByte, MsgPack.fromByte(0.toByte)),
        (1.toByte, MsgPack.fromInt(1)),
        (2.toByte, MsgPack.fromShort(2.toShort)),
        (3.toByte, MsgPack.fromLong(3L)),
        (4.toByte, MsgPack.fromBigInt(BigInt(4)))
      )
    }
  }

  test("Decoder[Byte] failure") {
    val msg = MsgPack.empty
    assert(decode[Byte](msg) === Left(TypeMismatchError("Byte", msg)))
  }

  test("Decoder[Short]") {
    check {
      Seq(
        (0.toShort, MsgPack.fromByte(0.toByte)),
        (1.toShort, MsgPack.fromInt(1)),
        (2.toShort, MsgPack.fromShort(2.toShort)),
        (3.toShort, MsgPack.fromLong(3L)),
        (4.toShort, MsgPack.fromBigInt(BigInt(4)))
      )
    }
  }

  test("Decoder[Short] failure") {
    val msg = MsgPack.empty
    assert(decode[Short](msg) === Left(TypeMismatchError("Short", msg)))
  }

  test("Decoder[Int]") {
    check {
      Seq(
        (0, MsgPack.fromByte(0.toByte)),
        (1, MsgPack.fromInt(1)),
        (2, MsgPack.fromShort(2.toShort)),
        (3, MsgPack.fromLong(3L)),
        (4, MsgPack.fromBigInt(BigInt(4)))
      )
    }
  }

  test("Decoder[Int] failure") {
    val msg = MsgPack.empty
    assert(decode[Int](msg) === Left(TypeMismatchError("Int", msg)))
  }

  test("Decoder[Long]") {
    check {
      Seq(
        (0L, MsgPack.fromByte(0.toByte)),
        (1L, MsgPack.fromInt(1)),
        (2L, MsgPack.fromShort(2.toShort)),
        (3L, MsgPack.fromLong(3L)),
        (4L, MsgPack.fromBigInt(BigInt(4)))
      )
    }
  }

  test("Decoder[Long] failure") {
    val msg = MsgPack.empty
    assert(decode[Long](msg) === Left(TypeMismatchError("Long", msg)))
  }

  test("Decoder[BigInt]") {
    check {
      Seq(
        (BigInt(0), MsgPack.fromByte(0.toByte)),
        (BigInt(1), MsgPack.fromInt(1)),
        (BigInt(2), MsgPack.fromShort(2.toShort)),
        (BigInt(3), MsgPack.fromLong(3L)),
        (BigInt(4), MsgPack.fromBigInt(BigInt(4)))
      )
    }
  }

  test("Decoder[BigInt] failure") {
    val msg = MsgPack.empty
    assert(decode[BigInt](msg) === Left(TypeMismatchError("BigInt", msg)))
  }

  test("Decoder[Double]") {
    check {
      Seq(
        (0.0, MsgPack.fromFloat(0.0f)),
        (0.1, MsgPack.fromDouble(0.1))
      )
    }
  }

  test("Decoder[Double] failure") {
    val msg = MsgPack.empty
    assert(decode[Double](msg) === Left(TypeMismatchError("Double", msg)))
  }

  test("Decoder[Float]") {
    check {
      Seq(
        (0.0f, MsgPack.fromFloat(0.0f)),
        (0.1f, MsgPack.fromDouble(0.1))
      )
    }
  }

  test("Decoder[Float] failure") {
    val msg = MsgPack.empty
    assert(decode[Float](msg) === Left(TypeMismatchError("Float", msg)))
  }

  test("Decoder[String]") {
    check {
      Seq(
        ("abjioeweuo", MsgPack.fromString("abjioeweuo"))
      )
    }
  }

  test("Decoder[String] failure") {
    val msg = MsgPack.empty
    assert(decode[String](msg) === Left(TypeMismatchError("String", msg)))
  }

  test("Decoder[Seq[A]]") {
    check {
      Seq(
        (Seq(0 to 14: _*), MsgPack.fromValues((0 to 14).map(MsgPack.fromInt): _*)),
        (Seq.empty[Int], MsgPack.empty)
      )
    }
  }

  test("Decoder[Seq[A]] failure") {
    val msg = MsgPack.fromString("")
    assert(decode[Seq[Int]](msg) === Left(TypeMismatchError("C[A]", msg)))
  }

  test("Decoder[Tuple2[A]]") {
    check {
      Seq(
        ((1, 2), MsgPack.fromValues(MsgPack.fromByte(1.toByte), MsgPack.fromByte(2.toByte)))
      )
    }
  }

  test("Decoder[Tuple2[A]] failure") {
    val msg = MsgPack.fromValues(MsgPack.fromInt(1), MsgPack.fromInt(2), MsgPack.fromInt(3))
    assert(decode[(Int, Int)](msg) === Left(TypeMismatchError("(A, B)", msg)))
  }

  test("Decoder[List[A]]") {
    check {
      Seq(
        (List(0 to 14: _*), MsgPack.fromValues((0 to 14).map(MsgPack.fromInt): _*))
      )
    }
  }

  test("Decoder[Vector[A]]") {
    check {
      Seq(
        (Vector(0 to 14: _*), MsgPack.fromValues((0 to 14).map(MsgPack.fromInt): _*))
      )
    }
  }

  test("Decoder[Map[A, B]]") {
    check {
      Seq(
        (('a' to 'z').zip(0 to 14).toMap, MsgPack.fromPairSeq(('a' to 'z').zip(0 to 14).map {
          case (k, v) => MsgPack.fromString(k.toString) -> MsgPack.fromByte(v.toByte)
        })),
        (Map.empty[Char, Int], MsgPack.empty)
      )
    }
  }

  test("Decoder[Map[A, B]] failure") {
    val msg = MsgPack.fromString("")
    assert(decode[Map[Int, Int]](msg) === Left(TypeMismatchError("M[K, V]", msg)))
  }

  test("Decoder[Map[A, Bar]]") {
    check {
      Seq(
        (
          ('a' to 'z').zip((0 to 14).map(a => Bar(a.toDouble))).toMap,
          MsgPack.fromPairSeq(('a' to 'z').zip(0 to 14).map {
            case (k, v) =>
              MsgPack.fromString(k.toString) -> MsgPack
                .fromPairs(MsgPack.fromString("double") -> MsgPack.fromDouble(v.toDouble))
          })
        )
      )
    }
  }

  test("Decoder[Qux]") {
    check {
      Seq(
        (Qux(None), MsgPack.fromPairs(MsgPack.fromString("byte")    -> MsgPack.nil)),
        (Qux(Some(1)), MsgPack.fromPairs(MsgPack.fromString("byte") -> MsgPack.fromByte(1.toByte)))
      )
    }
  }

  test("Decoder[Qux] failure") {
    assert(
      decode[Qux](MsgPack.fromString(" ")) === Left(TypeMismatchError("FieldType[K, H] :: T", MsgPack.fromString(" ")))
    )
    val a = decode[Qux](MsgPack.fromPairs(MsgPack.fromString("byte") -> MsgPack.fromString(" ")))
    assert(a === Left(TypeMismatchError("Int", MsgPack.fromString(" "))))
  }

  test("Decoder[Z]") {
    check {
      Seq[(Z, MsgPack)](
        (
          Z0(1),
          MsgPack.fromPairs(
            MsgPack.fromString("Z0") -> MsgPack.fromPairs(MsgPack.fromString("a") -> MsgPack.fromByte(1.toByte))
          )
        ),
        (
          Z1(2),
          MsgPack.fromPairs(
            MsgPack.fromString("Z1") -> MsgPack.fromPairs(MsgPack.fromString("b") -> MsgPack.fromByte(2.toByte))
          )
        )
      )
    }
  }

  test("Decoder[Z] failure") {
    val a = decode[Z](MsgPack.fromString(" "))
    assert(a === Left(TypeMismatchError("FieldType[K, L] :+: R", MsgPack.fromString(" "))))
    val b = decode[Z](MsgPack.fromPairs(MsgPack.fromString("Z2") -> MsgPack.fromInt(1)))
    assert(b === Left(TypeMismatchError("CNil", MsgPack.fromPairs(MsgPack.fromString("Z2") -> MsgPack.fromInt(1)))))
  }

  test("map") {
    val decode = Decoder[String].map(_.toInt)
    assert(decode(MsgPack.MString("30")).right.get === 30)
  }

  test("mapF") {
    val decode = Decoder[String].mapF(a => Right(a.toInt))
    assert(decode(MsgPack.MString("30")).right.get === 30)
  }

  test("flatMap") {
    val decode = Decoder[String]
      .flatMap(a => Decoder.lift(a.toInt))
      .flatMap(a => Decoder.liftF(Right(a.toDouble)))
    assert(decode(MsgPack.MString("30")).right.get === 30.0)
  }
}
