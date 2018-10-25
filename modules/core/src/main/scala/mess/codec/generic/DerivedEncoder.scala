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
      K: Witness.Aux[K],
      S: Encoder[K],
      H: Encoder[H],
      T: DerivedEncoder[T]): DerivedEncoder[FieldType[K, H] :: T] =
    new DerivedEncoder[FieldType[K, H] :: T] {
      def apply(a: FieldType[K, H] :: T): MsgPack =
        T(a.tail) match {
          case tt: MsgPack.MMap => tt.add(S(K.value), H(a.head))
          case tt               => tt
        }
    }

  implicit final val encodeCNil: DerivedEncoder[CNil] =
    new DerivedEncoder[CNil] {
      def apply(a: CNil): MsgPack = sys.error("Cannot encode CNil")
    }

  implicit final def encodeLabelledCCons[K <: Symbol, L, R <: Coproduct](
      implicit
      K: Witness.Aux[K],
      L: Encoder[L],
      R: DerivedEncoder[R]): DerivedEncoder[FieldType[K, L] :+: R] =
    new DerivedEncoder[FieldType[K, L] :+: R] {
      def apply(a: FieldType[K, L] :+: R): MsgPack = a match {
        case Inl(h) => MsgPack.MMap(mutable.HashMap.empty += MsgPack.MString(K.value.name) -> L(h))
        case Inr(t) => R(t)
      }
    }

  implicit final def encodeGen[A, R](implicit
                                     gen: LabelledGeneric.Aux[A, R],
                                     R: Lazy[DerivedEncoder[R]]): DerivedEncoder[A] =
    new DerivedEncoder[A] {
      def apply(a: A): MsgPack = R.value(gen.to(a))
    }
}
