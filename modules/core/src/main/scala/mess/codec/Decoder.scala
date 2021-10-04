package mess.codec

import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant

import mess.Fmt
import mess.internal.ScalaVersionSpecifics._

import scala.annotation.tailrec
import scala.collection.mutable

trait Decoder[A] extends Serializable { self =>

  def apply(m: Fmt): Either[DecodingFailure, A]

  final def map[B](f: A => B): Decoder[B] =
    m =>
      self(m) match {
        case Right(v) => Right(f(v))
        case Left(e)  => Left(e)
      }

  final def mapF[B](f: A => Either[DecodingFailure, B]): Decoder[B] =
    m =>
      self(m) match {
        case Right(v) => f(v)
        case Left(e)  => Left(e)
      }

  final def flatMap[B](f: A => Decoder[B]): Decoder[B] =
    m =>
      self(m) match {
        case Right(v) => f(v).apply(m)
        case Left(e)  => Left(e)
      }
}

object Decoder extends Decoder1 with MirrorDecoder with TupleDecoder {
  def apply[A](implicit A: Decoder[A]): Decoder[A] = A

  def lift[A](a: A): Decoder[A] = _ => Right(a)

  def liftF[A](a: Either[DecodingFailure, A]): Decoder[A] = _ => a
}

private[codec] trait Decoder1 {
  implicit val decodeFmt: Decoder[Fmt] = Right(_)

  implicit val decodeBoolean: Decoder[Boolean] = {
    case a: Fmt.MBool => Right(a.value)
    case m            => Left(TypeMismatchError("Boolean", m))
  }

  implicit val decodeBytes: Decoder[Array[Byte]] = {
    case a: Fmt.MExtension => Right(a.value)
    case a: Fmt.MBin       => Right(a.value)
    case a: Fmt.MString    => Right(a.value.getBytes(UTF_8))
    case m                 => Left(TypeMismatchError("Array[Byte]", m))
  }

  implicit val decodeByte: Decoder[Byte] = {
    case a: Fmt.MNumber => Right(a.asByte)
    case m              => Left(TypeMismatchError("Byte", m))
  }

  implicit val decodeShort: Decoder[Short] = {
    case a: Fmt.MNumber => Right(a.asShort)
    case m              => Left(TypeMismatchError("Short", m))
  }

  implicit val decodeInt: Decoder[Int] = {
    case a: Fmt.MNumber => Right(a.asInt)
    case m              => Left(TypeMismatchError("Int", m))
  }

  implicit val decodeLong: Decoder[Long] = {
    case a: Fmt.MNumber => Right(a.asLong)
    case m              => Left(TypeMismatchError("Long", m))
  }

  implicit val decodeBigInt: Decoder[BigInt] = {
    case a: Fmt.MNumber => Right(a.asBigInt)
    case m              => Left(TypeMismatchError("BigInt", m))
  }

  implicit val decodeDouble: Decoder[Double] = {
    case a: Fmt.MNumber => Right(a.asDouble)
    case m              => Left(TypeMismatchError("Double", m))
  }

  implicit val decodeFloat: Decoder[Float] = {
    case a: Fmt.MNumber => Right(a.asFloat)
    case m              => Left(TypeMismatchError("Float", m))
  }

  implicit val decodeChar: Decoder[Char] = {
    case Fmt.MString(a) if a.length == 1 => Right(a.head)
    case m                               => Left(TypeMismatchError("Char", m))
  }

  implicit val decodeString: Decoder[String] = {
    case Fmt.MString(a) => Right(a)
    case m              => Left(TypeMismatchError("String", m))
  }

  implicit val decodeTimestamp: Decoder[Instant] = {
    case Fmt.MTimestamp(a) => Right(a)
    case m                 => Left(TypeMismatchError("Timestamp", m))
  }

  implicit def decodeSome[A](implicit decodeA: Decoder[A]): Decoder[Some[A]] =
    m =>
      decodeA(m) match {
        case Right(v) => Right(Some(v))
        case Left(e)  => Left(e)
      }

  implicit val decodeNone: Decoder[None.type] =
    _ => Right(None)

  implicit def decodeOption[A](implicit A: Decoder[A]): Decoder[Option[A]] =
    m =>
      if (m == Fmt.MNil || m == Fmt.MUnit)
        Right(None)
      else
        A(m) match {
          case Right(v) => Right(Option(v))
          case Left(e)  => Left(e)
        }

  @inline private[this] def decodeContainer[C[_], A](implicit
    decodeA: Decoder[A],
    factoryA: Factory[A, C[A]]
  ): Decoder[C[A]] =
    m => {
      @tailrec def loop(it: Iterator[Fmt], b: mutable.Builder[A, C[A]]): Either[DecodingFailure, C[A]] =
        if (!it.hasNext) Right(b.result())
        else
          decodeA(it.next()) match {
            case Right(aa) => loop(it, b += aa)
            case Left(e)   => Left(e)
          }

      if (m == Fmt.MNil || m == Fmt.MUnit)
        Right(factoryA.newBuilder.result())
      else
        m match {
          case a: Fmt.MArray => loop(a.iterator, factoryA.newBuilder)
          case _             => Left(TypeMismatchError(s"C[A]", m))
        }
    }

  implicit def decodeSeq[A: Decoder]: Decoder[Seq[A]] = decodeContainer[Seq, A]

  implicit def decodeSet[A: Decoder]: Decoder[Set[A]] = decodeContainer[Set, A]

  implicit def decodeList[A: Decoder]: Decoder[List[A]] = decodeContainer[List, A]

  implicit def decodeVector[A: Decoder]: Decoder[Vector[A]] = decodeContainer[Vector, A]

  implicit def decodeMap[K, V](implicit
    decodeK: Decoder[K],
    decodeV: Decoder[V],
    factoryKV: Factory[(K, V), Map[K, V]]
  ): Decoder[Map[K, V]] =
    m => {
      @tailrec def loop(
        it: Iterator[(Fmt, Fmt)],
        b: mutable.Builder[(K, V), Map[K, V]]
      ): Either[DecodingFailure, Map[K, V]] =
        if (!it.hasNext) Right(b.result())
        else {
          val (k, v) = it.next()
          decodeK(k) match {
            case Right(kk) =>
              decodeV(v) match {
                case Right(vv) => loop(it, b += kk -> vv)
                case Left(e)   => Left(e)
              }
            case Left(e) => Left(e)
          }
        }

      if (m == Fmt.MNil || m == Fmt.MUnit)
        Right(factoryKV.newBuilder.result())
      else
        m match {
          case m: Fmt.MMap => loop(m.iterator, factoryKV.newBuilder)
          case _           => Left(TypeMismatchError(s"M[K, V]", m))
        }
    }
}
