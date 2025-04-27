package mess.codec

import mess.Fmt
import mess.MsgpackHelper
import mess.codec.semiauto.*

import scala.collection.Seq
import scala.collection.immutable

class DecoderSpec extends MsgpackHelper {
  case class Bar(double: Double)

  object Bar {
    implicit val decode: Decoder[Bar] = derivedDecoder[Bar]
  }

  case class Qux(byte: Option[Int])

  sealed trait Z

  case class Z0(a: Int) extends Z

  object Z0 {
    implicit val decode: Decoder[Z0] = derivedDecoder[Z0]
  }
  case class Z1(b: Int) extends Z

  object Z1 {
    implicit val decode: Decoder[Z1] = derivedDecoder[Z1]
  }

  object Qux {
    import mess.codec.semiauto._

    implicit val decode: Decoder[Qux] = derivedDecoder[Qux]
  }

  def decode[A](msg: Fmt)(implicit A: Decoder[A]): Either[Throwable, A] = A(msg)

  def check[A: Decoder](name: String, tc: Seq[(A, Fmt)]): Unit =
    test(name) {
      for ((expected, p) <- tc)
        assertEquals(decode[A](p).toTry.get, expected)
    }

  check("Decoder[Some[A]]",
        Seq(
          (Some(1), Fmt.fromInt(1))
        )
  )
  check("Decoder[Char]",
        Seq(
          ('a', Fmt.fromString("a"))
        )
  )
  test("Decoder[Array[Byte]]") {
    val m  = Fmt.fromBytes(Array(0x11, 0x1a).map(_.toByte))
    val xs = decode[Array[Byte]](m).toTry.get
    assertEquals(xs.toSeq, immutable.Seq(0x11, 0x1a).map(_.toByte))
  }
  test("Decoder[Char] failure") {
    val msg = Fmt.fromString("ab")
    assertEquals(decode[Char](msg), Left(TypeMismatchError("Char", msg)))
  }
  check("Decoder[None.type]",
        Seq(
          (None, Fmt.nil)
        )
  )
  check("Decoder[Option[A]]",
        Seq(
          (Option(1), Fmt.fromInt(1)),
          (Option.empty[Int], Fmt.nil)
        )
  )
  check("Decoder[Boolean]",
        Seq(
          (true, Fmt.fromBoolean(true)),
          (false, Fmt.fromBoolean(false))
        )
  )
  test("Decoder[Boolean] failure") {
    val msg = Fmt.unit
    assertEquals(decode[Boolean](msg), Left(TypeMismatchError("Boolean", msg)))
  }
  check(
    "Decoder[Byte]",
    Seq(
      (0.toByte, Fmt.fromByte(0.toByte)),
      (1.toByte, Fmt.fromInt(1)),
      (2.toByte, Fmt.fromShort(2.toShort)),
      (3.toByte, Fmt.fromLong(3L)),
      (4.toByte, Fmt.fromBigInt(BigInt(4)))
    )
  )
  test("Decoder[Byte] failure") {
    val msg = Fmt.unit
    assertEquals(decode[Byte](msg), Left(TypeMismatchError("Byte", msg)))
  }
  check(
    "Decoder[Short]",
    Seq(
      (0.toShort, Fmt.fromByte(0.toByte)),
      (1.toShort, Fmt.fromInt(1)),
      (2.toShort, Fmt.fromShort(2.toShort)),
      (3.toShort, Fmt.fromLong(3L)),
      (4.toShort, Fmt.fromBigInt(BigInt(4)))
    )
  )
  test("Decoder[Short] failure") {
    val msg = Fmt.unit
    assertEquals(decode[Short](msg), Left(TypeMismatchError("Short", msg)))
  }
  check("Decoder[Int]",
        Seq(
          (0, Fmt.fromByte(0.toByte)),
          (1, Fmt.fromInt(1)),
          (2, Fmt.fromShort(2.toShort)),
          (3, Fmt.fromLong(3L)),
          (4, Fmt.fromBigInt(BigInt(4)))
        )
  )
  test("Decoder[Int] failure") {
    val msg = Fmt.unit
    assertEquals(decode[Int](msg), Left(TypeMismatchError("Int", msg)))
  }
  check(
    "Decoder[Long]",
    Seq(
      (0L, Fmt.fromByte(0.toByte)),
      (1L, Fmt.fromInt(1)),
      (2L, Fmt.fromShort(2.toShort)),
      (3L, Fmt.fromLong(3L)),
      (4L, Fmt.fromBigInt(BigInt(4)))
    )
  )
  test("Decoder[Long] failure") {
    val msg = Fmt.unit
    assertEquals(decode[Long](msg), Left(TypeMismatchError("Long", msg)))
  }
  check(
    "Decoder[BigInt]",
    Seq(
      (BigInt(0), Fmt.fromByte(0.toByte)),
      (BigInt(1), Fmt.fromInt(1)),
      (BigInt(2), Fmt.fromShort(2.toShort)),
      (BigInt(3), Fmt.fromLong(3L)),
      (BigInt(4), Fmt.fromBigInt(BigInt(4)))
    )
  )
  test("Decoder[BigInt] failure") {
    val msg = Fmt.unit
    assertEquals(decode[BigInt](msg), Left(TypeMismatchError("BigInt", msg)))
  }
  check("Decoder[Double]",
        Seq(
          (0.0, Fmt.fromFloat(0.0f)),
          (0.1, Fmt.fromDouble(0.1))
        )
  )
  test("Decoder[Double] failure") {
    val msg = Fmt.unit
    assertEquals(decode[Double](msg), Left(TypeMismatchError("Double", msg)))
  }
  check("Decoder[Float]",
        Seq(
          (0.0f, Fmt.fromFloat(0.0f)),
          (0.1f, Fmt.fromDouble(0.1))
        )
  )
  test("Decoder[Float] failure") {
    val msg = Fmt.unit
    assertEquals(decode[Float](msg), Left(TypeMismatchError("Float", msg)))
  }
  check("Decoder[String]",
        Seq(
          ("abjioeweuo", Fmt.fromString("abjioeweuo"))
        )
  )
  test("Decoder[String] failure") {
    val msg = Fmt.unit
    assertEquals(decode[String](msg), Left(TypeMismatchError("String", msg)))
  }
  check("Decoder[Seq[A]]",
        Seq(
          (Seq(0 to 14: _*), Fmt.fromValues((0 to 14).map(Fmt.fromInt): _*)),
          (Seq.empty[Int], Fmt.unit)
        )
  )
  test("Decoder[Seq[A]] failure") {
    val msg = Fmt.fromString("")
    assertEquals(decode[Seq[Int]](msg), Left(TypeMismatchError("C[A]", msg)))
  }
  check("Decoder[immutable.Seq[A]]",
        Seq(
          (immutable.Seq(0 to 14: _*), Fmt.fromValues((0 to 14).map(Fmt.fromInt): _*)),
          (immutable.Seq.empty[Int], Fmt.unit)
        )
  )
  test("Decoder[immutable.Seq[A]] failure") {
    val msg = Fmt.fromString("")
    assertEquals(decode[immutable.Seq[Int]](msg), Left(TypeMismatchError("C[A]", msg)))
  }
  check("Decoder[Tuple2[A]]",
        Seq(
          ((1, 2), Fmt.fromValues(Fmt.fromByte(1.toByte), Fmt.fromByte(2.toByte)))
        )
  )
  test("Decoder[Tuple2[A]] failure") {
    val msg = Fmt.fromValues(Fmt.fromInt(1), Fmt.fromInt(2), Fmt.fromInt(3))
    assertEquals(decode[(Int, Int)](msg), Left(TypeMismatchError("(A, B)", msg)))
  }
  check("Decoder[List[A]]",
        Seq(
          (List(0 to 14: _*), Fmt.fromValues((0 to 14).map(Fmt.fromInt): _*))
        )
  )
  check("Decoder[Vector[A]]",
        Seq(
          (Vector(0 to 14: _*), Fmt.fromValues((0 to 14).map(Fmt.fromInt): _*))
        )
  )
  check(
    "Decoder[Map[A, B]]",
    Seq(
      (
        ('a' to 'z').zip(0 to 14).toMap,
        Fmt.fromEntries(('a' to 'z').zip(0 to 14).map { case (k, v) =>
          Fmt.fromString(k.toString) -> Fmt.fromByte(v.toByte)
        }: _*)
      ),
      (Map.empty[Char, Int], Fmt.unit)
    )
  )
  test("Decoder[Map[A, B]] failure") {
    val msg = Fmt.fromString("")
    assertEquals(decode[Map[Int, Int]](msg), Left(TypeMismatchError("M[K, V]", msg)))
  }
  check(
    "Decoder[Map[A, Bar]]",
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
    "Decoder[Qux]",
    Seq(
      (Qux(None), Fmt.fromEntries(Fmt.fromString("byte") -> Fmt.nil)),
      (Qux(Some(1)), Fmt.fromEntries(Fmt.fromString("byte") -> Fmt.fromByte(1.toByte)))
    )
  )
  test("Decoder[Qux] failure") {
    assertEquals(
      decode[Qux](Fmt.fromString(" ")),
      Left(TypeMismatchError("Product", Fmt.fromString(" ")))
    )
    val a = decode[Qux](Fmt.fromEntries(Fmt.fromString("byte") -> Fmt.fromString(" ")))
    assertEquals(a, Left(TypeMismatchError("Int", Fmt.fromString(" "))))
  }
  locally {
    import mess.codec.auto._
    check(
      "Decoder[Z]",
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
    )
  }
  test("Decoder[Z] failure") {
    import mess.codec.auto._

    val a = decode[Z](Fmt.fromString(" "))
    assertEquals(a, Left(TypeMismatchError("Sum", Fmt.fromString(" "))))
    val b = decode[Z](Fmt.fromEntries(Fmt.fromString("Z2") -> Fmt.fromInt(1)))
    assertEquals(b, Left(TypeMismatchError("Sum", Fmt.fromEntries(Fmt.fromString("Z2") -> Fmt.fromInt(1)))))
  }

  test("map") {
    val decode = Decoder[String].map(_.toInt)
    assertEquals(decode(Fmt.MString("30")), Right(30))
  }

  test("mapF") {
    val decode = Decoder[String].mapF(a => Right(a.toInt))
    assertEquals(decode(Fmt.MString("30")), Right(30))
  }

  test("flatMap") {
    val decode = Decoder[String]
      .flatMap(a => Decoder.lift(a.toInt))
      .flatMap(a => Decoder.liftF(Right(a.toDouble)))
    assertEquals(decode(Fmt.MString("30")), Right(30.0))
  }
}
