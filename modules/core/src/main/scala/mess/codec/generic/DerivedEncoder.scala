package mess.codec.generic

import export._
import mess.Encoder
import mess.ast.MsgPack
import shapeless._
import shapeless.labelled.FieldType

import scala.collection.mutable

trait DerivedEncoder[A] extends Encoder[A]

@exports
object DerivedEncoder extends LowPriorityDerivedEncoder

private[mess] trait LowPriorityDerivedEncoder {

  implicit final val encodeHNil: DerivedEncoder[HNil] =
    new DerivedEncoder[HNil] {
      def apply(a: HNil): MsgPack = MsgPack.MMap(mutable.HashMap.empty)
    }

  implicit final def encodeLabelledHList[K <: Symbol, H, T <: HList](
      implicit
      witK: Witness.Aux[K],
      encodeK: Encoder[K],
      encodeH: Encoder[H],
      encodeT: DerivedEncoder[T]): DerivedEncoder[FieldType[K, H] :: T] =
    new DerivedEncoder[FieldType[K, H] :: T] {
      def apply(a: FieldType[K, H] :: T): MsgPack =
        encodeT(a.tail) match {
          case tt: MsgPack.MMap => tt.add(encodeK(witK.value), encodeH(a.head))
          case tt               => tt
        }
    }

  implicit final val encodeCNil: DerivedEncoder[CNil] =
    new DerivedEncoder[CNil] {
      def apply(a: CNil): MsgPack = sys.error("Cannot encode CNil")
    }

  implicit final def encodeLabelledCCons[K <: Symbol, L, R <: Coproduct](
      implicit
      witK: Witness.Aux[K],
      encodeL: Encoder[L],
      encodeR: DerivedEncoder[R]): DerivedEncoder[FieldType[K, L] :+: R] =
    new DerivedEncoder[FieldType[K, L] :+: R] {
      def apply(a: FieldType[K, L] :+: R): MsgPack = a match {
        case Inl(h) => MsgPack.MMap(mutable.HashMap.empty += MsgPack.fromString(witK.value.name) -> encodeL(h))
        case Inr(t) => encodeR(t)
      }
    }

  implicit final def encodeGen[A, R](implicit
                                     gen: LabelledGeneric.Aux[A, R],
                                     encodeR: Lazy[DerivedEncoder[R]]): DerivedEncoder[A] =
    new DerivedEncoder[A] {
      def apply(a: A): MsgPack = encodeR.value(gen.to(a))
    }
}
