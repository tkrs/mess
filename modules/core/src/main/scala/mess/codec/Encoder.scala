package mess.codec

import mess.Fmt

import scala.annotation.tailrec
import scala.collection.mutable

trait Encoder[A] extends Serializable {
  self =>

  def apply(a: A): Fmt

  final def contramap[B](f: B => A): Encoder[B] = a => self(f(a))

  final def map(f: Fmt => Fmt): Encoder[A] = a => f(self(a))
}

object Encoder extends Encoder1 with TupleEncoder {
  def apply[A](implicit A: Encoder[A]): Encoder[A] = A

  final def instance[A](fa: A => Fmt): Encoder[A] = a => fa(a)
}

trait Encoder1 {
  implicit final val encodeBoolean: Encoder[Boolean] = a => Fmt.fromBoolean(a)

  implicit final val encodeBytes: Encoder[Array[Byte]] =
    Encoder.instance[Array[Byte]](Fmt.fromBytes)

  implicit final val encodeByte: Encoder[Byte] = Fmt.fromByte(_)

  implicit final val encodeShort: Encoder[Short] = Fmt.fromShort(_)

  implicit final val encodeInt: Encoder[Int] = Fmt.fromInt(_)

  implicit final val encodeLong: Encoder[Long] = Fmt.fromLong(_)

  implicit final val encodeBigInt: Encoder[BigInt] = Fmt.fromBigInt(_)

  implicit final val encodeDouble: Encoder[Double] = Fmt.fromDouble(_)

  implicit final val encodeFloat: Encoder[Float] = Fmt.fromFloat(_)

  implicit final val encodeChar: Encoder[Char] = a => Fmt.fromString(a.toString)

  implicit final val encodeString: Encoder[String] = Fmt.fromString(_)

  implicit final def encodeSymbol[K <: Symbol]: Encoder[K] = a => Fmt.fromString(a.name)

  implicit final def encodeOption[A](implicit A: Encoder[A]): Encoder[Option[A]] = {
    case Some(v) => A(v)
    case None    => Fmt.MNil
  }

  implicit final def encodeSome[A](implicit A: Encoder[A]): Encoder[Some[A]] =
    A.contramap[Some[A]](_.get)

  implicit final val encodeNone: Encoder[None.type] = _ => Fmt.MNil

  @tailrec private[this] def iterLoop[A](rem: Iterator[A], acc: mutable.Builder[Fmt, Vector[Fmt]])(
    implicit A: Encoder[A]
  ): Vector[Fmt] =
    if (!rem.hasNext) acc.result()
    else iterLoop(rem, acc += A(rem.next()))

  implicit final def encodeSeq[A: Encoder]: Encoder[Seq[A]] =
    a => Fmt.fromVector(iterLoop(a.iterator, Vector.newBuilder))

  implicit final def encodeSet[A: Encoder]: Encoder[Set[A]] =
    a => Fmt.fromVector(iterLoop(a.iterator, Vector.newBuilder))

  implicit final def encodeList[A: Encoder]: Encoder[List[A]] =
    a => Fmt.fromVector(iterLoop(a.iterator, Vector.newBuilder))

  implicit final def encodeVector[A: Encoder]: Encoder[Vector[A]] =
    a => Fmt.fromVector(iterLoop(a.iterator, Vector.newBuilder))

  @tailrec private[this] def mapLoop[K, V](
    it: Iterator[(K, V)],
    acc: mutable.Builder[(Fmt, Fmt), Seq[(Fmt, Fmt)]]
  )(
    implicit
    encodeK: Encoder[K],
    encodeV: Encoder[V]
  ): Seq[(Fmt, Fmt)] =
    if (!it.hasNext) acc.result()
    else {
      val (k, v) = it.next()
      mapLoop(it, acc += encodeK(k) -> encodeV(v))
    }

  implicit final def encodeMap[K: Encoder, V: Encoder]: Encoder[Map[K, V]] =
    a => Fmt.fromEntries(mapLoop(a.iterator, Seq.newBuilder): _*)
}
