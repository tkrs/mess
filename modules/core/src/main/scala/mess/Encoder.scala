package mess

import mess.ast.MsgPack
import shapeless._
import shapeless.labelled.FieldType

import scala.annotation.tailrec
import scala.collection.mutable

trait Encoder[A] extends Serializable { self =>

  def apply(a: A): MsgPack

  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    def apply(a: B): MsgPack = self(f(a))
  }

  final def map(f: MsgPack => MsgPack): Encoder[A] = new Encoder[A] {
    def apply(a: A): MsgPack = f(self(a))
  }
}

object Encoder extends HighPriorityEncoder with TupleEncoder {

  def apply[A](implicit A: Encoder[A]): Encoder[A] = A

  final def instance[A](fa: A => MsgPack): Encoder[A] = new Encoder[A] {
    def apply(a: A): MsgPack = fa(a)
  }
}

trait HighPriorityEncoder extends LowPriorityEncoder {

  implicit final val encodeBoolean: Encoder[Boolean] = new Encoder[Boolean] {
    def apply(a: Boolean): MsgPack = MsgPack.fromBoolean(a)
  }

  implicit final val encodeBytes: Encoder[Array[Byte]] =
    Encoder.instance[Array[Byte]](MsgPack.fromBytes)

  implicit final val encodeByte: Encoder[Byte] = new Encoder[Byte] {
    def apply(a: Byte): MsgPack = MsgPack.fromByte(a)
  }

  implicit final val encodeShort: Encoder[Short] = new Encoder[Short] {
    def apply(a: Short): MsgPack = MsgPack.fromShort(a)
  }

  implicit final val encodeInt: Encoder[Int] = new Encoder[Int] {
    def apply(a: Int): MsgPack = MsgPack.fromInt(a)
  }

  implicit final val encodeLong: Encoder[Long] = new Encoder[Long] {
    def apply(a: Long): MsgPack = MsgPack.fromLong(a)
  }

  implicit final val encodeBigInt: Encoder[BigInt] = new Encoder[BigInt] {
    def apply(a: BigInt): MsgPack = MsgPack.fromBigInt(a)
  }

  implicit final val encodeDouble: Encoder[Double] = new Encoder[Double] {
    def apply(a: Double): MsgPack = MsgPack.fromDouble(a)
  }

  implicit final val encodeFloat: Encoder[Float] = new Encoder[Float] {
    def apply(a: Float): MsgPack = MsgPack.fromFloat(a)
  }

  implicit final val encodeChar: Encoder[Char] = new Encoder[Char] {
    def apply(a: Char): MsgPack = MsgPack.fromString(a.toString)
  }

  implicit final val encodeString: Encoder[String] = new Encoder[String] {
    def apply(a: String): MsgPack = MsgPack.fromString(a)
  }

  implicit final def encodeSymbol[K <: Symbol]: Encoder[K] = new Encoder[K] {
    def apply(a: K): MsgPack = MsgPack.fromString(a.name)
  }

  implicit final def encodeOption[A](implicit A: Encoder[A]): Encoder[Option[A]] = new Encoder[Option[A]] {
    def apply(a: Option[A]): MsgPack = a match {
      case Some(v) => A(v)
      case None    => MsgPack.nil
    }
  }

  implicit final def encodeSome[A](implicit A: Encoder[A]): Encoder[Some[A]] =
    A.contramap[Some[A]](_.get)

  implicit final val encodeNone: Encoder[None.type] = new Encoder[None.type] {
    def apply(a: None.type): MsgPack = MsgPack.nil
  }

  @tailrec private[this] def iterLoop[A](rem: Iterator[A], acc: mutable.Builder[MsgPack, Vector[MsgPack]])(
    implicit A: Encoder[A]
  ): Vector[MsgPack] =
    if (!rem.hasNext) acc.result()
    else iterLoop(rem, acc += A(rem.next()))

  implicit final def encodeSeq[A: Encoder]: Encoder[Seq[A]] = new Encoder[Seq[A]] {
    def apply(a: Seq[A]): MsgPack =
      MsgPack.fromVector(iterLoop(a.iterator, Vector.newBuilder))
  }

  implicit final def encodeSet[A: Encoder]: Encoder[Set[A]] = new Encoder[Set[A]] {
    def apply(a: Set[A]): MsgPack =
      MsgPack.fromVector(iterLoop(a.iterator, Vector.newBuilder))
  }

  implicit final def encodeList[A: Encoder]: Encoder[List[A]] = new Encoder[List[A]] {
    def apply(a: List[A]): MsgPack =
      MsgPack.fromVector(iterLoop(a.iterator, Vector.newBuilder))
  }

  implicit final def encodeVector[A: Encoder]: Encoder[Vector[A]] = new Encoder[Vector[A]] {
    def apply(a: Vector[A]): MsgPack =
      MsgPack.fromVector(iterLoop(a.iterator, Vector.newBuilder))
  }

  @tailrec private[this] def mapLoop[K, V](
    it: Iterator[(K, V)],
    acc: mutable.Builder[(MsgPack, MsgPack), Seq[(MsgPack, MsgPack)]]
  )(
    implicit
    encodeK: Encoder[K],
    encodeV: Encoder[V]
  ): Seq[(MsgPack, MsgPack)] =
    if (!it.hasNext) acc.result()
    else {
      val (k, v) = it.next()
      mapLoop(it, acc += encodeK(k) -> encodeV(v))
    }

  implicit final def encodeMap[K: Encoder, V: Encoder]: Encoder[Map[K, V]] =
    new Encoder[Map[K, V]] {
      def apply(a: Map[K, V]): MsgPack =
        MsgPack.fromPairSeq(mapLoop(a.iterator, Seq.newBuilder))
    }
}

trait LowPriorityEncoder {

  implicit final val encodeHNil: Encoder[HNil] =
    new Encoder[HNil] {
      def apply(a: HNil): MsgPack = MsgPack.MMap(mutable.HashMap.empty)
    }

  implicit final def encodeLabelledHList[K <: Symbol, H, T <: HList](
    implicit
    witK: Witness.Aux[K],
    encodeK: Encoder[K],
    encodeH: Encoder[H],
    encodeT: Lazy[Encoder[T]]
  ): Encoder[FieldType[K, H] :: T] =
    new Encoder[FieldType[K, H] :: T] {
      def apply(a: FieldType[K, H] :: T): MsgPack =
        encodeT.value(a.tail) match {
          case tt: MsgPack.MMap => tt.add(encodeK(witK.value), encodeH(a.head))
          case tt               => tt
        }
    }

  implicit final val encodeCNil: Encoder[CNil] =
    new Encoder[CNil] {
      def apply(a: CNil): MsgPack = sys.error("Cannot encode CNil")
    }

  implicit final def encodeLabelledCCons[K <: Symbol, L, R <: Coproduct](
    implicit
    witK: Witness.Aux[K],
    encodeL: Encoder[L],
    encodeR: Lazy[Encoder[R]]
  ): Encoder[FieldType[K, L] :+: R] =
    new Encoder[FieldType[K, L] :+: R] {
      def apply(a: FieldType[K, L] :+: R): MsgPack = a match {
        case Inl(h) => MsgPack.MMap(mutable.HashMap.empty += MsgPack.fromString(witK.value.name) -> encodeL(h))
        case Inr(t) => encodeR.value(t)
      }
    }

  implicit final def encodeGen[A, R](
    implicit
    gen: LabelledGeneric.Aux[A, R],
    encodeR: Lazy[Encoder[R]]
  ): Encoder[A] =
    new Encoder[A] {
      def apply(a: A): MsgPack = encodeR.value(gen.to(a))
    }
}
