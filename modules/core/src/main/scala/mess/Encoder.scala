package mess

import shapeless._
import shapeless.labelled.FieldType

import scala.annotation.tailrec
import scala.collection.mutable

trait Encoder[A] extends Serializable { self =>

  def apply(a: A): Fmt

  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    def apply(a: B): Fmt = self(f(a))
  }

  final def map(f: Fmt => Fmt): Encoder[A] = new Encoder[A] {
    def apply(a: A): Fmt = f(self(a))
  }
}

object Encoder extends Encoder1 with TupleEncoder {

  def apply[A](implicit A: Encoder[A]): Encoder[A] = A

  final def instance[A](fa: A => Fmt): Encoder[A] = new Encoder[A] {
    def apply(a: A): Fmt = fa(a)
  }
}

trait Encoder1 extends Encoder2 {

  implicit final val encodeBoolean: Encoder[Boolean] = new Encoder[Boolean] {
    def apply(a: Boolean): Fmt = Fmt.fromBoolean(a)
  }

  implicit final val encodeBytes: Encoder[Array[Byte]] =
    Encoder.instance[Array[Byte]](Fmt.fromBytes)

  implicit final val encodeByte: Encoder[Byte] = new Encoder[Byte] {
    def apply(a: Byte): Fmt = Fmt.fromByte(a)
  }

  implicit final val encodeShort: Encoder[Short] = new Encoder[Short] {
    def apply(a: Short): Fmt = Fmt.fromShort(a)
  }

  implicit final val encodeInt: Encoder[Int] = new Encoder[Int] {
    def apply(a: Int): Fmt = Fmt.fromInt(a)
  }

  implicit final val encodeLong: Encoder[Long] = new Encoder[Long] {
    def apply(a: Long): Fmt = Fmt.fromLong(a)
  }

  implicit final val encodeBigInt: Encoder[BigInt] = new Encoder[BigInt] {
    def apply(a: BigInt): Fmt = Fmt.fromBigInt(a)
  }

  implicit final val encodeDouble: Encoder[Double] = new Encoder[Double] {
    def apply(a: Double): Fmt = Fmt.fromDouble(a)
  }

  implicit final val encodeFloat: Encoder[Float] = new Encoder[Float] {
    def apply(a: Float): Fmt = Fmt.fromFloat(a)
  }

  implicit final val encodeChar: Encoder[Char] = new Encoder[Char] {
    def apply(a: Char): Fmt = Fmt.fromString(a.toString)
  }

  implicit final val encodeString: Encoder[String] = new Encoder[String] {
    def apply(a: String): Fmt = Fmt.fromString(a)
  }

  implicit final def encodeSymbol[K <: Symbol]: Encoder[K] = new Encoder[K] {
    def apply(a: K): Fmt = Fmt.fromString(a.name)
  }

  implicit final def encodeOption[A](implicit A: Encoder[A]): Encoder[Option[A]] = new Encoder[Option[A]] {
    def apply(a: Option[A]): Fmt = a match {
      case Some(v) => A(v)
      case None    => Fmt.MNil
    }
  }

  implicit final def encodeSome[A](implicit A: Encoder[A]): Encoder[Some[A]] =
    A.contramap[Some[A]](_.get)

  implicit final val encodeNone: Encoder[None.type] = new Encoder[None.type] {
    def apply(a: None.type): Fmt = Fmt.MNil
  }

  @tailrec private[this] def iterLoop[A](rem: Iterator[A], acc: mutable.Builder[Fmt, Vector[Fmt]])(
    implicit A: Encoder[A]
  ): Vector[Fmt] =
    if (!rem.hasNext) acc.result()
    else iterLoop(rem, acc += A(rem.next()))

  implicit final def encodeSeq[A: Encoder]: Encoder[Seq[A]] = new Encoder[Seq[A]] {
    def apply(a: Seq[A]): Fmt =
      Fmt.fromVector(iterLoop(a.iterator, Vector.newBuilder))
  }

  implicit final def encodeSet[A: Encoder]: Encoder[Set[A]] = new Encoder[Set[A]] {
    def apply(a: Set[A]): Fmt =
      Fmt.fromVector(iterLoop(a.iterator, Vector.newBuilder))
  }

  implicit final def encodeList[A: Encoder]: Encoder[List[A]] = new Encoder[List[A]] {
    def apply(a: List[A]): Fmt =
      Fmt.fromVector(iterLoop(a.iterator, Vector.newBuilder))
  }

  implicit final def encodeVector[A: Encoder]: Encoder[Vector[A]] = new Encoder[Vector[A]] {
    def apply(a: Vector[A]): Fmt =
      Fmt.fromVector(iterLoop(a.iterator, Vector.newBuilder))
  }

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
    new Encoder[Map[K, V]] {
      def apply(a: Map[K, V]): Fmt =
        Fmt.fromEntries(mapLoop(a.iterator, Seq.newBuilder): _*)
    }
}

trait Encoder2 {

  implicit final val encodeHNil: Encoder[HNil] =
    new Encoder[HNil] {
      def apply(a: HNil): Fmt = Fmt.MMap.newBuilder
    }

  implicit final def encodeLabelledHList[K <: Symbol, H, T <: HList](
    implicit
    witK: Witness.Aux[K],
    encodeK: Encoder[K],
    encodeH: Encoder[H],
    encodeT: Lazy[Encoder[T]]
  ): Encoder[FieldType[K, H] :: T] =
    new Encoder[FieldType[K, H] :: T] {
      def apply(a: FieldType[K, H] :: T): Fmt =
        encodeT.value(a.tail) match {
          case tt: Fmt.MMap => tt + (encodeK(witK.value) -> encodeH(a.head))
          case tt           => tt
        }
    }

  implicit final val encodeCNil: Encoder[CNil] =
    new Encoder[CNil] {
      def apply(a: CNil): Fmt = sys.error("Cannot encode CNil")
    }

  implicit final def encodeLabelledCCons[K <: Symbol, L, R <: Coproduct](
    implicit
    witK: Witness.Aux[K],
    encodeL: Encoder[L],
    encodeR: Lazy[Encoder[R]]
  ): Encoder[FieldType[K, L] :+: R] =
    new Encoder[FieldType[K, L] :+: R] {
      def apply(a: FieldType[K, L] :+: R): Fmt = a match {
        case Inl(h) => Fmt.fromEntries(Fmt.fromString(witK.value.name) -> encodeL(h))
        case Inr(t) => encodeR.value(t)
      }
    }

  implicit final def encodeGen[A, R](
    implicit
    gen: LabelledGeneric.Aux[A, R],
    encodeR: Lazy[Encoder[R]]
  ): Encoder[A] =
    new Encoder[A] {
      def apply(a: A): Fmt = encodeR.value(gen.to(a))
    }
}
