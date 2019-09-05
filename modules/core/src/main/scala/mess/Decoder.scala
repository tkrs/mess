package mess

import java.nio.charset.StandardCharsets.UTF_8

import mess.internal.ScalaVersionSpecifics._
import shapeless._
import shapeless.labelled.{field, FieldType}

import scala.annotation.tailrec
import scala.collection.mutable

trait Decoder[A] extends Serializable { self =>

  def apply(m: Fmt): Decoder.Result[A]

  final def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    def apply(m: Fmt): Decoder.Result[B] =
      self(m) match {
        case Right(v) => Right(f(v))
        case Left(e)  => Left(e)
      }
  }

  final def mapF[B](f: A => Decoder.Result[B]): Decoder[B] = new Decoder[B] {
    def apply(m: Fmt): Decoder.Result[B] =
      self(m) match {
        case Right(v) => f(v)
        case Left(e)  => Left(e)
      }
  }

  final def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    def apply(m: Fmt): Decoder.Result[B] =
      self(m) match {
        case Right(v) => f(v).apply(m)
        case Left(e)  => Left(e)
      }
  }
}

object Decoder extends Decoder1 with TupleDecoder {

  def apply[A](implicit A: Decoder[A]): Decoder[A] = A

  def lift[A](a: A): Decoder[A] = new Decoder[A] {
    def apply(_m: Fmt): Result[A] = Right(a)
  }

  def liftF[A](a: Result[A]): Decoder[A] = new Decoder[A] {
    def apply(_m: Fmt): Result[A] = a
  }
}

trait Decoder1 extends Decoder2 {
  type Result[A] = Either[DecodingFailure, A]

  implicit val decodeFmt: Decoder[Fmt] = new Decoder[Fmt] {
    def apply(m: Fmt): Result[Fmt] = Right(m)
  }

  implicit val decodeBoolean: Decoder[Boolean] = new Decoder[Boolean] {
    def apply(m: Fmt): Result[Boolean] = m match {
      case a: Fmt.MBool => Right(a.value)
      case _            => Left(TypeMismatchError("Boolean", m))
    }
  }

  implicit val decodeBytes: Decoder[Array[Byte]] = new Decoder[Array[Byte]] {
    def apply(m: Fmt): Result[Array[Byte]] = m match {
      case a: Fmt.MExtension => Right(a.value)
      case a: Fmt.MBin       => Right(a.value)
      case a: Fmt.MString    => Right(a.value.getBytes(UTF_8))
      case _                 => Left(TypeMismatchError("Array[Byte]", m))
    }
  }

  implicit val decodeByte: Decoder[Byte] = new Decoder[Byte] {
    def apply(m: Fmt): Result[Byte] = m match {
      case a: Fmt.MNumber => Right(a.asByte)
      case _              => Left(TypeMismatchError("Byte", m))
    }
  }

  implicit val decodeShort: Decoder[Short] = new Decoder[Short] {
    def apply(m: Fmt): Result[Short] = m match {
      case a: Fmt.MNumber => Right(a.asShort)
      case _              => Left(TypeMismatchError("Short", m))
    }
  }

  implicit val decodeInt: Decoder[Int] = new Decoder[Int] {
    def apply(m: Fmt): Result[Int] = m match {
      case a: Fmt.MNumber => Right(a.asInt)
      case _              => Left(TypeMismatchError("Int", m))
    }
  }

  implicit val decodeLong: Decoder[Long] = new Decoder[Long] {
    def apply(m: Fmt): Result[Long] = m match {
      case a: Fmt.MNumber => Right(a.asLong)
      case _              => Left(TypeMismatchError("Long", m))
    }
  }

  implicit val decodeBigInt: Decoder[BigInt] = new Decoder[BigInt] {
    def apply(m: Fmt): Result[BigInt] = m match {
      case a: Fmt.MNumber => Right(a.asBigInt)
      case _              => Left(TypeMismatchError("BigInt", m))
    }
  }

  implicit val decodeDouble: Decoder[Double] = new Decoder[Double] {
    def apply(m: Fmt): Result[Double] = m match {
      case a: Fmt.MNumber => Right(a.asDouble)
      case _              => Left(TypeMismatchError("Double", m))
    }
  }

  implicit val decodeFloat: Decoder[Float] = new Decoder[Float] {
    def apply(m: Fmt): Result[Float] = m match {
      case a: Fmt.MNumber => Right(a.asFloat)
      case _              => Left(TypeMismatchError("Float", m))
    }
  }

  implicit val decodeChar: Decoder[Char] = new Decoder[Char] {
    def apply(m: Fmt): Result[Char] = m match {
      case Fmt.MString(a) if a.length == 1 => Right(a.head)
      case _                               => Left(TypeMismatchError("Char", m))
    }
  }

  implicit val decodeString: Decoder[String] = new Decoder[String] {
    def apply(m: Fmt): Result[String] = m match {
      case Fmt.MString(a) => Right(a)
      case _              => Left(TypeMismatchError("String", m))
    }
  }

  implicit def decodeSome[A](implicit decodeA: Decoder[A]): Decoder[Some[A]] =
    new Decoder[Some[A]] {
      def apply(m: Fmt): Result[Some[A]] =
        decodeA(m) match {
          case Right(v) => Right(Some(v))
          case Left(e)  => Left(e)
        }
    }

  implicit val decodeNone: Decoder[None.type] =
    new Decoder[None.type] {
      def apply(m: Fmt): Result[None.type] = Right(None)
    }

  implicit def decodeOption[A](implicit A: Decoder[A]): Decoder[Option[A]] =
    new Decoder[Option[A]] {
      def apply(m: Fmt): Result[Option[A]] =
        if (m == Fmt.MNil || m == Fmt.MUnit)
          Right(None)
        else
          A(m) match {
            case Right(v) => Right(Option(v))
            case Left(e)  => Left(e)
          }
    }

  @inline private[this] def decodeContainer[C[_], A](
    implicit
    decodeA: Decoder[A],
    factoryA: Factory[A, C[A]]
  ): Decoder[C[A]] =
    new Decoder[C[A]] {
      def apply(m: Fmt): Result[C[A]] = {
        @tailrec def loop(it: Iterator[Fmt], b: mutable.Builder[A, C[A]]): Result[C[A]] =
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
    }

  implicit def decodeSeq[A: Decoder]: Decoder[Seq[A]]       = decodeContainer[Seq, A]
  implicit def decodeSet[A: Decoder]: Decoder[Set[A]]       = decodeContainer[Set, A]
  implicit def decodeList[A: Decoder]: Decoder[List[A]]     = decodeContainer[List, A]
  implicit def decodeVector[A: Decoder]: Decoder[Vector[A]] = decodeContainer[Vector, A]

  implicit def decodeMapLike[M[_, _] <: Map[K, V], K, V](
    implicit
    decodeK: Decoder[K],
    decodeV: Decoder[V],
    factoryKV: Factory[(K, V), M[K, V]]
  ): Decoder[M[K, V]] =
    new Decoder[M[K, V]] {
      def apply(m: Fmt): Result[M[K, V]] = {
        @tailrec def loop(it: Iterator[(Fmt, Fmt)], b: mutable.Builder[(K, V), M[K, V]]): Result[M[K, V]] =
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
}

trait Decoder2 {

  implicit final val decodeHNil: Decoder[HNil] =
    new Decoder[HNil] {
      def apply(a: Fmt): Decoder.Result[HNil] = Right(HNil)
    }

  implicit final def decodeLabelledHList[K <: Symbol, H, T <: HList](
    implicit
    witK: Witness.Aux[K],
    decodeH: Lazy[Decoder[H]],
    decodeT: Lazy[Decoder[T]]
  ): Decoder[FieldType[K, H] :: T] =
    new Decoder[FieldType[K, H] :: T] {
      def apply(m: Fmt): Decoder.Result[FieldType[K, H] :: T] = m match {
        case a: Fmt.MMap =>
          decodeT.value(m) match {
            case Right(t) =>
              val v = a.get(Fmt.MString(witK.value.name)).getOrElse(Fmt.MNil)
              decodeH.value(v) match {
                case Right(h) => Right(field[K](h) :: t)
                case Left(e)  => Left(e)
              }
            case Left(e) => Left(e)
          }
        case _ => Left(TypeMismatchError("FieldType[K, H] :: T", m))
      }
    }

  implicit final val decodeCNil: Decoder[CNil] =
    new Decoder[CNil] {
      def apply(m: Fmt): Decoder.Result[CNil] =
        Left(TypeMismatchError("CNil", m))
    }

  implicit final def decodeLabelledCCons[K <: Symbol, L, R <: Coproduct](
    implicit
    witK: Witness.Aux[K],
    decodeL: Lazy[Decoder[L]],
    decodeR: Lazy[Decoder[R]]
  ): Decoder[FieldType[K, L] :+: R] =
    new Decoder[FieldType[K, L] :+: R] {
      def apply(m: Fmt): Decoder.Result[FieldType[K, L] :+: R] = m match {
        case a: Fmt.MMap =>
          val v = a.get(Fmt.fromString(witK.value.name)).getOrElse(Fmt.MNil)
          decodeL.value.map(v => Inl(field[K](v))).apply(v) match {
            case r @ Right(_) => r
            case Left(_)      => decodeR.value.map(vv => Inr(vv)).apply(m)
          }
        case _ => Left(TypeMismatchError("FieldType[K, L] :+: R", m))
      }
    }

  implicit final def decodeGen[A, R](
    implicit
    gen: LabelledGeneric.Aux[A, R],
    decodeR: Lazy[Decoder[R]]
  ): Decoder[A] =
    new Decoder[A] {
      def apply(a: Fmt): Decoder.Result[A] = decodeR.value(a) match {
        case Right(v) => Right(gen.from(v))
        case Left(e)  => Left(e)
      }
    }
}
