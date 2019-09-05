package mess

import org.scalatest.FunSuite

class DecoderSpec extends FunSuite with MsgpackHelper {

  case class Bar(double: Double)
  case class Foo(int: Int, str: String, bar: Bar)
  case class Qux(byte: Option[Int])

  sealed trait Z
  case class Z0(a: Int) extends Z
  case class Z1(b: Int) extends Z

  def decode[A](msg: Fmt)(implicit A: Decoder[A]): Either[Throwable, A] = A(msg)

  def check[A: Decoder](tc: Seq[(A, Fmt)]): Unit =
    for ((expected, p) <- tc) {
      assert(decode[A](p).toTry.get === expected)
    }

  test("Decoder[Some[A]]") {
    check {
      Seq(
        (Some(1), Fmt.fromInt(1))
      )
    }
  }

  test("Decoder[Char]") {
    check {
      Seq(
        ('a', Fmt.fromString("a"))
      )
    }
  }

  test("Decoder[Array[Byte]]") {
    check {
      Seq(
        (Array(0x11, 0x1a).map(_.toByte), Fmt.fromBytes(Array(0x11, 0x1a).map(_.toByte)))
      )
    }
  }

  test("Decoder[Char] failure") {
    val msg = Fmt.fromString("ab")
    assert(decode[Char](msg) === Left(TypeMismatchError("Char", msg)))
  }

  test("Decoder[None.type]") {
    check {
      Seq(
        (None, Fmt.nil)
      )
    }
  }

  test("Decoder[Option[A]]") {
    check {
      Seq(
        (Option(1), Fmt.fromInt(1)),
        (Option.empty[Int], Fmt.nil)
      )
    }
  }

  test("Decoder[Boolean]") {
    check {
      Seq(
        (true, Fmt.fromBoolean(true)),
        (false, Fmt.fromBoolean(false))
      )
    }
  }

  test("Decoder[Boolean] failure") {
    val msg = Fmt.unit
    assert(decode[Boolean](msg) === Left(TypeMismatchError("Boolean", msg)))
  }

  test("Decoder[Byte]") {
    check {
      Seq(
        (0.toByte, Fmt.fromByte(0.toByte)),
        (1.toByte, Fmt.fromInt(1)),
        (2.toByte, Fmt.fromShort(2.toShort)),
        (3.toByte, Fmt.fromLong(3L)),
        (4.toByte, Fmt.fromBigInt(BigInt(4)))
      )
    }
  }

  test("Decoder[Byte] failure") {
    val msg = Fmt.unit
    assert(decode[Byte](msg) === Left(TypeMismatchError("Byte", msg)))
  }

  test("Decoder[Short]") {
    check {
      Seq(
        (0.toShort, Fmt.fromByte(0.toByte)),
        (1.toShort, Fmt.fromInt(1)),
        (2.toShort, Fmt.fromShort(2.toShort)),
        (3.toShort, Fmt.fromLong(3L)),
        (4.toShort, Fmt.fromBigInt(BigInt(4)))
      )
    }
  }

  test("Decoder[Short] failure") {
    val msg = Fmt.unit
    assert(decode[Short](msg) === Left(TypeMismatchError("Short", msg)))
  }

  test("Decoder[Int]") {
    check {
      Seq(
        (0, Fmt.fromByte(0.toByte)),
        (1, Fmt.fromInt(1)),
        (2, Fmt.fromShort(2.toShort)),
        (3, Fmt.fromLong(3L)),
        (4, Fmt.fromBigInt(BigInt(4)))
      )
    }
  }

  test("Decoder[Int] failure") {
    val msg = Fmt.unit
    assert(decode[Int](msg) === Left(TypeMismatchError("Int", msg)))
  }

  test("Decoder[Long]") {
    check {
      Seq(
        (0L, Fmt.fromByte(0.toByte)),
        (1L, Fmt.fromInt(1)),
        (2L, Fmt.fromShort(2.toShort)),
        (3L, Fmt.fromLong(3L)),
        (4L, Fmt.fromBigInt(BigInt(4)))
      )
    }
  }

  test("Decoder[Long] failure") {
    val msg = Fmt.unit
    assert(decode[Long](msg) === Left(TypeMismatchError("Long", msg)))
  }

  test("Decoder[BigInt]") {
    check {
      Seq(
        (BigInt(0), Fmt.fromByte(0.toByte)),
        (BigInt(1), Fmt.fromInt(1)),
        (BigInt(2), Fmt.fromShort(2.toShort)),
        (BigInt(3), Fmt.fromLong(3L)),
        (BigInt(4), Fmt.fromBigInt(BigInt(4)))
      )
    }
  }

  test("Decoder[BigInt] failure") {
    val msg = Fmt.unit
    assert(decode[BigInt](msg) === Left(TypeMismatchError("BigInt", msg)))
  }

  test("Decoder[Double]") {
    check {
      Seq(
        (0.0, Fmt.fromFloat(0.0f)),
        (0.1, Fmt.fromDouble(0.1))
      )
    }
  }

  test("Decoder[Double] failure") {
    val msg = Fmt.unit
    assert(decode[Double](msg) === Left(TypeMismatchError("Double", msg)))
  }

  test("Decoder[Float]") {
    check {
      Seq(
        (0.0f, Fmt.fromFloat(0.0f)),
        (0.1f, Fmt.fromDouble(0.1))
      )
    }
  }

  test("Decoder[Float] failure") {
    val msg = Fmt.unit
    assert(decode[Float](msg) === Left(TypeMismatchError("Float", msg)))
  }

  test("Decoder[String]") {
    check {
      Seq(
        ("abjioeweuo", Fmt.fromString("abjioeweuo"))
      )
    }
  }

  test("Decoder[String] failure") {
    val msg = Fmt.unit
    assert(decode[String](msg) === Left(TypeMismatchError("String", msg)))
  }

  test("Decoder[Seq[A]]") {
    check {
      Seq(
        (Seq(0 to 14: _*), Fmt.fromValues((0 to 14).map(Fmt.fromInt): _*)),
        (Seq.empty[Int], Fmt.unit)
      )
    }
  }

  test("Decoder[Seq[A]] failure") {
    val msg = Fmt.fromString("")
    assert(decode[Seq[Int]](msg) === Left(TypeMismatchError("C[A]", msg)))
  }

  test("Decoder[Tuple2[A]]") {
    check {
      Seq(
        ((1, 2), Fmt.fromValues(Fmt.fromByte(1.toByte), Fmt.fromByte(2.toByte)))
      )
    }
  }

  test("Decoder[Tuple2[A]] failure") {
    val msg = Fmt.fromValues(Fmt.fromInt(1), Fmt.fromInt(2), Fmt.fromInt(3))
    assert(decode[(Int, Int)](msg) === Left(TypeMismatchError("(A, B)", msg)))
  }

  test("Decoder[List[A]]") {
    check {
      Seq(
        (List(0 to 14: _*), Fmt.fromValues((0 to 14).map(Fmt.fromInt): _*))
      )
    }
  }

  test("Decoder[Vector[A]]") {
    check {
      Seq(
        (Vector(0 to 14: _*), Fmt.fromValues((0 to 14).map(Fmt.fromInt): _*))
      )
    }
  }

  test("Decoder[Map[A, B]]") {
    check {
      Seq(
        (('a' to 'z').zip(0 to 14).toMap, Fmt.fromEntries(('a' to 'z').zip(0 to 14).map {
          case (k, v) => Fmt.fromString(k.toString) -> Fmt.fromByte(v.toByte)
        }: _*)),
        (Map.empty[Char, Int], Fmt.unit)
      )
    }
  }

  test("Decoder[Map[A, B]] failure") {
    val msg = Fmt.fromString("")
    assert(decode[Map[Int, Int]](msg) === Left(TypeMismatchError("M[K, V]", msg)))
  }

  test("Decoder[Map[A, Bar]]") {
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

  test("Decoder[Qux]") {
    check {
      Seq(
        (Qux(None), Fmt.fromEntries(Fmt.fromString("byte")    -> Fmt.nil)),
        (Qux(Some(1)), Fmt.fromEntries(Fmt.fromString("byte") -> Fmt.fromByte(1.toByte)))
      )
    }
  }

  test("Decoder[Qux] failure") {
    assert(
      decode[Qux](Fmt.fromString(" ")) === Left(TypeMismatchError("FieldType[K, H] :: T", Fmt.fromString(" ")))
    )
    val a = decode[Qux](Fmt.fromEntries(Fmt.fromString("byte") -> Fmt.fromString(" ")))
    assert(a === Left(TypeMismatchError("Int", Fmt.fromString(" "))))
  }

  test("Decoder[Z]") {
    check {
      Seq[(Z, Fmt)](
        (
          Z0(1),
          Fmt.fromEntries(
            Fmt.fromString("Z0") -> Fmt.fromEntries(Fmt.fromString("a") -> Fmt.fromByte(1.toByte))
          )
        ),
        (
          Z1(2),
          Fmt.fromEntries(
            Fmt.fromString("Z1") -> Fmt.fromEntries(Fmt.fromString("b") -> Fmt.fromByte(2.toByte))
          )
        )
      )
    }
  }

  test("Decoder[Z] failure") {
    val a = decode[Z](Fmt.fromString(" "))
    assert(a === Left(TypeMismatchError("FieldType[K, L] :+: R", Fmt.fromString(" "))))
    val b = decode[Z](Fmt.fromEntries(Fmt.fromString("Z2") -> Fmt.fromInt(1)))
    assert(b === Left(TypeMismatchError("CNil", Fmt.fromEntries(Fmt.fromString("Z2") -> Fmt.fromInt(1)))))
  }

  test("map") {
    val decode = Decoder[String].map(_.toInt)
    assert(decode(Fmt.MString("30")) === Right(30))
  }

  test("mapF") {
    val decode = Decoder[String].mapF(a => Right(a.toInt))
    assert(decode(Fmt.MString("30")) === Right(30))
  }

  test("flatMap") {
    val decode = Decoder[String]
      .flatMap(a => Decoder.lift(a.toInt))
      .flatMap(a => Decoder.liftF(Right(a.toDouble)))
    assert(decode(Fmt.MString("30")) === Right(30.0))
  }
}
