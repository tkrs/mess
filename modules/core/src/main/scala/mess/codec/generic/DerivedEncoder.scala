package mess.codec.generic

import export._
import mess.Encoder
import mess.ast.MsgPack
import shapeless._
import shapeless.labelled.FieldType

import scala.collection.mutable

trait DerivedEncoder[A] extends Encoder[A] {
  def apply(a: A): MsgPack = MsgPack.fromMap(mapBuilder(a).result())
  def mapBuilder(a: A): mutable.Builder[(MsgPack, MsgPack), Map[MsgPack, MsgPack]]
}

@exports
object DerivedEncoder extends LowPriorityDerivedEncoder

private[mess] trait LowPriorityDerivedEncoder {

  private type Result = mutable.Builder[(MsgPack, MsgPack), Map[MsgPack, MsgPack]]

  implicit final val encodeHNil: DerivedEncoder[HNil] =
    new DerivedEncoder[HNil] {
      def mapBuilder(a: HNil): Result = Map.newBuilder
    }

  implicit final def encodeLabelledHList[K <: Symbol, H, T <: HList](
      implicit
      witK: Witness.Aux[K],
      encodeK: Encoder[K],
      encodeH: Encoder[H],
      encodeT: DerivedEncoder[T]): DerivedEncoder[FieldType[K, H] :: T] =
    new DerivedEncoder[FieldType[K, H] :: T] {
      def mapBuilder(a: FieldType[K, H] :: T): Result =
        encodeT.mapBuilder(a.tail) += encodeK(witK.value) -> encodeH(a.head)
    }

  implicit final val encodeCNil: DerivedEncoder[CNil] =
    new DerivedEncoder[CNil] {
      override def apply(a: CNil): MsgPack = sys.error("Cannot encode CNil")
      def mapBuilder(a: CNil): Result      = sys.error("Should be not invoked")
    }

  implicit final def encodeLabelledCCons[K <: Symbol, L, R <: Coproduct](
      implicit
      witK: Witness.Aux[K],
      encodeL: Encoder[L],
      encodeR: DerivedEncoder[R]): DerivedEncoder[FieldType[K, L] :+: R] =
    new DerivedEncoder[FieldType[K, L] :+: R] {
      override def apply(a: FieldType[K, L] :+: R): MsgPack = a match {
        case Inl(h) => MsgPack.fromPairs(MsgPack.fromString(witK.value.name) -> encodeL(h))
        case Inr(t) => encodeR(t)
      }
      def mapBuilder(a: FieldType[K, L] :+: R): Result = sys.error("Should be not invoked")
    }

  implicit final def encodeGen[A, R](implicit
                                     gen: LabelledGeneric.Aux[A, R],
                                     encodeR: Lazy[DerivedEncoder[R]]): DerivedEncoder[A] =
    new DerivedEncoder[A] {
      override def apply(a: A): MsgPack = encodeR.value(gen.to(a))
      def mapBuilder(a: A): Result      = encodeR.value.mapBuilder(gen.to(a))
    }
}
