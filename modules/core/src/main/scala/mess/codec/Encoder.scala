package mess.codec

import mess.Fmt

import scala.annotation.tailrec
import scala.collection.mutable

trait Encoder[A] extends Serializable { self =>
  def apply(a: A): Fmt

  final def contramap[B](f: B => A): Encoder[B] = a => self(f(a))

  final def map(f: Fmt => Fmt): Encoder[A] = a => f(self(a))
}

object Encoder extends Encoder1 with TupleEncoder {
  trait AsArray[A] extends Encoder[A] {
    def applyToArray(a: A): Fmt.MArray

    final def apply(a: A): Fmt                                  = applyToArray(a)
    final def mapArray(f: Fmt.MArray => Fmt.MArray): AsArray[A] = a => f(applyToArray(a))
  }

  trait AsMap[A] extends Encoder[A] {
    def applyToMap(a: A): Fmt.MMap

    final def apply(a: A): Fmt                          = applyToMap(a)
    final def mapMap(f: Fmt.MMap => Fmt.MMap): AsMap[A] = a => f(applyToMap(a))
  }

  def apply[A](implicit A: Encoder[A]): Encoder[A]                   = A
  def asMap[A](implicit A: Encoder.AsMap[A]): Encoder.AsMap[A]       = A
  def asArray[A](implicit A: Encoder.AsArray[A]): Encoder.AsArray[A] = A

  final def instance[A](fa: A => Fmt): Encoder[A] = a => fa(a)
}

private[codec] trait Encoder1 {
  implicit final val encodeBoolean: Encoder[Boolean]   = Fmt.fromBoolean(_)
  implicit final val encodeBytes: Encoder[Array[Byte]] = Fmt.fromBytes(_)
  implicit final val encodeByte: Encoder[Byte]         = Fmt.fromByte(_)
  implicit final val encodeShort: Encoder[Short]       = Fmt.fromShort(_)
  implicit final val encodeInt: Encoder[Int]           = Fmt.fromInt(_)
  implicit final val encodeLong: Encoder[Long]         = Fmt.fromLong(_)
  implicit final val encodeBigInt: Encoder[BigInt]     = Fmt.fromBigInt(_)
  implicit final val encodeDouble: Encoder[Double]     = Fmt.fromDouble(_)
  implicit final val encodeFloat: Encoder[Float]       = Fmt.fromFloat(_)
  implicit final val encodeChar: Encoder[Char]         = a => Fmt.fromString(a.toString)
  implicit final val encodeString: Encoder[String]     = Fmt.fromString(_)

  implicit final def encodeSymbol[K <: Symbol]: Encoder[K] = a => Fmt.fromString(a.name)

  implicit final def encodeOption[A](implicit A: Encoder[A]): Encoder[Option[A]] = {
    case Some(v) => A(v)
    case None    => Fmt.nil
  }

  implicit final def encodeSome[A](implicit A: Encoder[A]): Encoder[Some[A]] =
    A.contramap[Some[A]](_.get)

  implicit final val encodeNone: Encoder[None.type] = _ => Fmt.nil

  @tailrec private[this] def buildVector[A](rem: Iterator[A], acc: mutable.Builder[Fmt, Vector[Fmt]])(
    implicit A: Encoder[A]
  ): Vector[Fmt] =
    if (!rem.hasNext) acc.result()
    else buildVector(rem, acc += A(rem.next()))

  implicit final def encodeSeq[A: Encoder]: Encoder.AsArray[Seq[A]] =
    a => Fmt.MArray(buildVector(a.iterator, Vector.newBuilder))

  implicit final def encodeSet[A: Encoder]: Encoder.AsArray[Set[A]] =
    a => Fmt.MArray(buildVector(a.iterator, Vector.newBuilder))

  implicit final def encodeList[A: Encoder]: Encoder.AsArray[List[A]] =
    a => Fmt.MArray(buildVector(a.iterator, Vector.newBuilder))

  implicit final def encodeVector[A: Encoder]: Encoder.AsArray[Vector[A]] =
    a => Fmt.MArray(buildVector(a.iterator, Vector.newBuilder))

  @tailrec private[this] def buildMap[K, V](
    it: Iterator[(K, V)],
    acc: mutable.Builder[(Fmt, Fmt), Map[Fmt, Fmt]]
  )(
    implicit
    encodeK: Encoder[K],
    encodeV: Encoder[V]
  ): Map[Fmt, Fmt] =
    if (!it.hasNext) acc.result()
    else {
      val (k, v) = it.next()
      buildMap(it, acc += encodeK(k) -> encodeV(v))
    }

  implicit final def encodeMapAsMaps[K: Encoder, V: Encoder]: Encoder.AsMap[Map[K, V]] =
    a => Fmt.MMap(buildMap(a.iterator, Map.newBuilder))
}
