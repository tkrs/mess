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

  def check[A: Decoder](tc: Seq[(A, MsgPack)]): Unit = {
    for ((expected, p) <- tc) {
      decode(p) match {
        case Right(v) =>
          assert(v === expected)
        case Left(e) =>
          throw e
      }
    }
  }

  test("Decoder[Some[A]]") {
    check {
      Seq(
        (Some(1), MsgPack.mInt(1))
      )
    }
  }

  test("Decoder[Char]") {
    check {
      Seq(
        ('a', MsgPack.mStr("a"))
      )
    }
  }

  test("Decoder[None.type]") {
    check {
      Seq(
        (None, MsgPack.mNil)
      )
    }
  }

  test("Decoder[Option[A]]") {
    check {
      Seq(
        (Option(1), MsgPack.mInt(1)),
        (Option.empty[Int], MsgPack.mNil)
      )
    }
  }

  test("Decoder[Boolean]") {
    check {
      Seq(
        (true, MsgPack.True),
        (false, MsgPack.False)
      )
    }
  }
  test("Decoder[Boolean] failure") {
    val msg = MsgPack.mEmpty
    assert(decode[Boolean](msg) === Left(TypeMismatchError("Boolean", msg)))
  }

  test("Decoder[Byte]") {
    check {
      Seq(
        (0.toByte, MsgPack.mByte(0.toByte)),
        (1.toByte, MsgPack.mInt(1)),
        (2.toByte, MsgPack.mShort(2.toShort)),
        (3.toByte, MsgPack.mLong(3L)),
        (4.toByte, MsgPack.mBigInt(BigInt(4)))
      )
    }
  }
  test("Decoder[Byte] failure") {
    val msg = MsgPack.mEmpty
    assert(decode[Byte](msg) === Left(TypeMismatchError("Byte", msg)))
  }

  test("Decoder[Short]") {
    check {
      Seq(
        (0.toShort, MsgPack.mByte(0.toByte)),
        (1.toShort, MsgPack.mInt(1)),
        (2.toShort, MsgPack.mShort(2.toShort)),
        (3.toShort, MsgPack.mLong(3L)),
        (4.toShort, MsgPack.mBigInt(BigInt(4)))
      )
    }
  }
  test("Decoder[Short] failure") {
    val msg = MsgPack.mEmpty
    assert(decode[Short](msg) === Left(TypeMismatchError("Short", msg)))
  }

  test("Decoder[Int]") {
    check {
      Seq(
        (0, MsgPack.mByte(0.toByte)),
        (1, MsgPack.mInt(1)),
        (2, MsgPack.mShort(2.toShort)),
        (3, MsgPack.mLong(3L)),
        (4, MsgPack.mBigInt(BigInt(4)))
      )
    }
  }
  test("Decoder[Int] failure") {
    val msg = MsgPack.mEmpty
    assert(decode[Int](msg) === Left(TypeMismatchError("Int", msg)))
  }

  test("Decoder[Long]") {
    check {
      Seq(
        (0L, MsgPack.mByte(0.toByte)),
        (1L, MsgPack.mInt(1)),
        (2L, MsgPack.mShort(2.toShort)),
        (3L, MsgPack.mLong(3L)),
        (4L, MsgPack.mBigInt(BigInt(4)))
      )
    }
  }
  test("Decoder[Long] failure") {
    val msg = MsgPack.mEmpty
    assert(decode[Long](msg) === Left(TypeMismatchError("Long", msg)))
  }

  test("Decoder[BigInt]") {
    check {
      Seq(
        (BigInt(0), MsgPack.mByte(0.toByte)),
        (BigInt(1), MsgPack.mInt(1)),
        (BigInt(2), MsgPack.mShort(2.toShort)),
        (BigInt(3), MsgPack.mLong(3L)),
        (BigInt(4), MsgPack.mBigInt(BigInt(4)))
      )
    }
  }
  test("Decoder[BigInt] failure") {
    val msg = MsgPack.mEmpty
    assert(decode[BigInt](msg) === Left(TypeMismatchError("BigInt", msg)))
  }

  test("Decoder[Double]") {
    check {
      Seq(
        (0.0, MsgPack.mFloat(0.0f)),
        (0.1, MsgPack.mDouble(0.1))
      )
    }
  }
  test("Decoder[Double] failure") {
    val msg = MsgPack.mEmpty
    assert(decode[Double](msg) === Left(TypeMismatchError("Double", msg)))
  }

  test("Decoder[Float]") {
    check {
      Seq(
        (0.0f, MsgPack.mFloat(0.0f)),
        (0.1f, MsgPack.mDouble(0.1))
      )
    }
  }
  test("Decoder[Float] failure") {
    val msg = MsgPack.mEmpty
    assert(decode[Float](msg) === Left(TypeMismatchError("Float", msg)))
  }

  test("Decoder[Seq[A]]") {
    check {
      Seq(
        (Seq(0 to 14: _*), MsgPack.mArr((0 to 14).map(MsgPack.mInt): _*)),
        (Seq.empty[Int], MsgPack.mEmpty)
      )
    }
  }
  test("Decoder[Seq[A]] failure") {
    val msg = MsgPack.mStr("")
    assert(decode[Seq[Int]](msg) === Left(TypeMismatchError("C[A]", msg)))
  }

  test("Decoder[List[A]]") {
    check {
      Seq(
        (List(0 to 14: _*), MsgPack.mArr((0 to 14).map(MsgPack.mInt): _*))
      )
    }
  }

  test("Decoder[Vector[A]]") {
    check {
      Seq(
        (Vector(0 to 14: _*), MsgPack.mArr((0 to 14).map(MsgPack.mInt): _*))
      )
    }
  }

  test("Decoder[Map[A, B]]") {
    check {
      Seq(
        (('a' to 'z').zip(0 to 14).toMap, MsgPack.mMap(('a' to 'z').zip(0 to 14).map {
          case (k, v) => MsgPack.mStr(k.toString) -> MsgPack.mByte(v.toByte)
        }: _*)),
        (Map.empty[Char, Int], MsgPack.mEmpty)
      )
    }
  }
  test("Decoder[Map[A, B]] failure") {
    val msg = MsgPack.mStr("")
    assert(decode[Map[Int, Int]](msg) === Left(TypeMismatchError("M[K, V]", msg)))
  }

  test("Decoder[Map[A, Bar]]") {
    check {
      Seq(
        (('a' to 'z').zip((0 to 14).map(a => Bar(a.toDouble))).toMap, MsgPack.mMap(('a' to 'z').zip(0 to 14).map {
          case (k, v) => MsgPack.mStr(k.toString) -> MsgPack.mMap(MsgPack.mStr("double") -> MsgPack.mDouble(v.toDouble))
        }: _*))
      )
    }
  }

  test("Decoder[Qux]") {
    check {
      Seq(
        (Qux(None), MsgPack.mMap(MsgPack.mStr("byte")    -> MsgPack.mNil)),
        (Qux(Some(1)), MsgPack.mMap(MsgPack.mStr("byte") -> MsgPack.mByte(1.toByte)))
      )
    }
  }

  test("Decoder[Qux] failure") {
    assert(decode[Qux](MsgPack.mStr(" ")) === Left(TypeMismatchError("FieldType[K, H] :: T", MsgPack.mStr(" "))))
    val a = decode[Qux](MsgPack.mMap(MsgPack.mStr("byte") -> MsgPack.mStr(" ")))
    assert(a === Left(TypeMismatchError("Int", MsgPack.mStr(" "))))
  }

  test("Decoder[Z]") {
    check {
      Seq[(Z, MsgPack)](
        (Z0(1), MsgPack.mMap(MsgPack.mStr("Z0") -> MsgPack.mMap(MsgPack.mStr("a") -> MsgPack.mByte(1.toByte)))),
        (Z1(2), MsgPack.mMap(MsgPack.mStr("Z1") -> MsgPack.mMap(MsgPack.mStr("b") -> MsgPack.mByte(2.toByte))))
      )
    }
  }

  test("Decoder[Z] failure") {
    val a = decode[Z](MsgPack.mStr(" "))
    assert(a === Left(TypeMismatchError("FieldType[K, L] :+: R", MsgPack.mStr(" "))))
    val b = decode[Z](MsgPack.mMap(MsgPack.mStr("Z2") -> MsgPack.mInt(1)))
    assert(b === Left(TypeMismatchError("CNil", MsgPack.mMap(MsgPack.mStr("Z2") -> MsgPack.mInt(1)))))
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
