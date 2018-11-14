package mess

import export.imports
import mess.ast.MsgPack
import mess.internal.ScalaVersionSpecifics._

import scala.annotation.tailrec
import scala.collection.mutable

trait Decoder[A] extends Serializable { self =>

  def apply(m: MsgPack): Decoder.Result[A]

  final def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    def apply(m: MsgPack): Decoder.Result[B] =
      self(m) match {
        case Right(v) => Right(f(v))
        case Left(e)  => Left(e)
      }
  }

  final def mapF[B](f: A => Decoder.Result[B]): Decoder[B] = new Decoder[B] {
    def apply(m: MsgPack): Decoder.Result[B] =
      self(m) match {
        case Right(v) => f(v)
        case Left(e)  => Left(e)
      }
  }

  final def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    def apply(m: MsgPack): Decoder.Result[B] =
      self(m) match {
        case Right(v) => f(v).apply(m)
        case Left(e)  => Left(e)
      }
  }
}

object Decoder extends LowPriorityDecoder with TupleDecoder {

  type Result[A] = Either[DecodingFailure, A]

  @inline def apply[A](implicit A: Decoder[A]): Decoder[A] = A

  def lift[A](a: A): Decoder[A] = new Decoder[A] {
    def apply(_m: MsgPack): Result[A] = Right(a)
  }

  def liftF[A](a: Result[A]): Decoder[A] = new Decoder[A] {
    def apply(_m: MsgPack): Result[A] = a
  }

  implicit val decodeBoolean: Decoder[Boolean] = new Decoder[Boolean] {
    def apply(m: MsgPack): Result[Boolean] = m.asBoolean match {
      case Some(a) => Right(a)
      case _       => Left(TypeMismatchError("Boolean", m))
    }
  }

  implicit val decodeBytes: Decoder[Array[Byte]] = new Decoder[Array[Byte]] {
    def apply(m: MsgPack): Result[Array[Byte]] = m.asByteArray match {
      case Some(a) => Right(a)
      case _       => Left(TypeMismatchError("Array[Byte]", m))
    }
  }

  implicit val decodeByte: Decoder[Byte] = new Decoder[Byte] {
    def apply(m: MsgPack): Result[Byte] = m.asByte match {
      case Some(a) => Right(a)
      case _       => Left(TypeMismatchError("Byte", m))
    }
  }

  implicit val decodeShort: Decoder[Short] = new Decoder[Short] {
    def apply(m: MsgPack): Result[Short] = m.asShort match {
      case Some(a) => Right(a)
      case _       => Left(TypeMismatchError("Short", m))
    }
  }

  implicit val decodeInt: Decoder[Int] = new Decoder[Int] {
    def apply(m: MsgPack): Result[Int] = m.asInt match {
      case Some(a) => Right(a)
      case _       => Left(TypeMismatchError("Int", m))
    }
  }

  implicit val decodeLong: Decoder[Long] = new Decoder[Long] {
    def apply(m: MsgPack): Result[Long] = m.asLong match {
      case Some(a) => Right(a)
      case _       => Left(TypeMismatchError("Long", m))
    }
  }

  implicit val decodeBigInt: Decoder[BigInt] = new Decoder[BigInt] {
    def apply(m: MsgPack): Result[BigInt] = m.asBigInt match {
      case Some(a) => Right(a)
      case _       => Left(TypeMismatchError("BigInt", m))
    }
  }

  implicit val decodeDouble: Decoder[Double] = new Decoder[Double] {
    def apply(m: MsgPack): Result[Double] = m.asDouble match {
      case Some(a) => Right(a)
      case _       => Left(TypeMismatchError("Double", m))
    }
  }

  implicit val decodeFloat: Decoder[Float] = new Decoder[Float] {
    def apply(m: MsgPack): Result[Float] = m.asFloat match {
      case Some(a) => Right(a)
      case _       => Left(TypeMismatchError("Float", m))
    }
  }

  implicit val decodeChar: Decoder[Char] = new Decoder[Char] {
    def apply(m: MsgPack): Result[Char] = m.asChar match {
      case Some(a) => Right(a)
      case _       => Left(TypeMismatchError("Char", m))
    }
  }

  implicit val decodeString: Decoder[String] = new Decoder[String] {
    def apply(m: MsgPack): Result[String] = m.asString match {
      case Some(a) => Right(a)
      case _       => Left(TypeMismatchError("String", m))
    }
  }

  implicit def decodeSome[A](implicit A: Decoder[A]): Decoder[Some[A]] =
    new Decoder[Some[A]] {
      def apply(m: MsgPack): Result[Some[A]] =
        A(m) match {
          case Right(v) => Right(Some(v))
          case Left(e)  => Left(e)
        }
    }

  implicit val decodeNone: Decoder[None.type] =
    new Decoder[None.type] {
      def apply(m: MsgPack): Result[None.type] = Right(None)
    }

  implicit def decodeOption[A](implicit A: Decoder[A]): Decoder[Option[A]] =
    new Decoder[Option[A]] {
      def apply(m: MsgPack): Result[Option[A]] = m match {
        case MsgPack.MNil | MsgPack.MEmpty => Right(None)
        case _ =>
          A(m) match {
            case Right(v) => Right(Option(v))
            case Left(e)  => Left(e)
          }
      }
    }

  @inline private[this] def decodeContainer[C[_], A](implicit A: Decoder[A], cbf: Factory[A, C[A]]): Decoder[C[A]] =
    new Decoder[C[A]] {
      def apply(m: MsgPack): Result[C[A]] = {
        @tailrec def loop(it: Iterator[MsgPack], b: mutable.Builder[A, C[A]]): Result[C[A]] = {
          if (!it.hasNext) Right(b.result())
          else
            A.apply(it.next()) match {
              case Right(aa) => loop(it, b += aa)
              case Left(e)   => Left(e)
            }
        }

        m match {
          case MsgPack.MNil | MsgPack.MEmpty => Right(cbf.newBuilder.result())
          case MsgPack.MArray(a)             => loop(a.iterator, cbf.newBuilder)
          case _                             => Left(TypeMismatchError(s"C[A]", m))
        }
      }
    }

  implicit def decodeSeq[A: Decoder]: Decoder[Seq[A]]       = decodeContainer[Seq, A]
  implicit def decodeSet[A: Decoder]: Decoder[Set[A]]       = decodeContainer[Set, A]
  implicit def decodeList[A: Decoder]: Decoder[List[A]]     = decodeContainer[List, A]
  implicit def decodeVector[A: Decoder]: Decoder[Vector[A]] = decodeContainer[Vector, A]

  implicit def decodeMapLike[M[_, _] <: Map[K, V], K, V](implicit
                                                         K: Decoder[K],
                                                         V: Decoder[V],
                                                         cbf: Factory[(K, V), M[K, V]]): Decoder[M[K, V]] =
    new Decoder[M[K, V]] {
      def apply(m: MsgPack): Result[M[K, V]] = {
        @tailrec def loop(it: Iterator[(MsgPack, MsgPack)], b: mutable.Builder[(K, V), M[K, V]]): Result[M[K, V]] = {
          if (!it.hasNext) Right(b.result())
          else {
            val (k, v) = it.next()
            K.apply(k) match {
              case Right(kk) =>
                V.apply(v) match {
                  case Right(vv) => loop(it, b += kk -> vv)
                  case Left(e)   => Left(e)
                }
              case Left(e) => Left(e)
            }
          }
        }

        m match {
          case MsgPack.MNil | MsgPack.MEmpty => Right(cbf.newBuilder.result())
          case MsgPack.MMap(a)               => loop(a.iterator, cbf.newBuilder)
          case _                             => Left(TypeMismatchError(s"M[K, V]", m))
        }
      }
    }
}

@imports[Decoder]
trait LowPriorityDecoder
