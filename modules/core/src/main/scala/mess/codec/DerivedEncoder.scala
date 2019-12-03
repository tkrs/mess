package mess.codec

import mess.Fmt
import shapeless._
import shapeless.labelled._

trait DerivedEncoder[A] extends Encoder.AsMap[A]

object DerivedEncoder extends DerivedEncoder1

trait DerivedEncoder1 extends DerivedEncoder2 {
  implicit final def encodeGen[A, R](
    implicit
    gen: LabelledGeneric.Aux[A, R],
    encodeR: Lazy[DerivedEncoder[R]]
  ): DerivedEncoder[A] =
    a => encodeR.value.applyToMap(gen.to(a))
}

trait DerivedEncoder2 extends DerivedEncoder3 {
  implicit final val encodeHNil: DerivedEncoder[HNil] =
    _ => Fmt.MMap.newBuilder

  implicit final def encodeLabelledHList[K <: Symbol, H, T <: HList](
    implicit
    witK: Witness.Aux[K],
    encodeK: Encoder[K],
    encodeH: Encoder[H],
    encodeT: Lazy[DerivedEncoder[T]]
  ): DerivedEncoder[FieldType[K, H] :: T] =
    a => encodeT.value.applyToMap(a.tail) + (encodeK(witK.value) -> encodeH(a.head))
}

trait DerivedEncoder3 {
  implicit final val encodeCNil: DerivedEncoder[CNil] =
    _ => sys.error("Cannot encode CNil")

  implicit final def encodeLabelledCCons[K <: Symbol, L, R <: Coproduct](
    implicit
    witK: Witness.Aux[K],
    encodeL: Lazy[DerivedEncoder[L]],
    encodeR: Lazy[DerivedEncoder[R]]
  ): DerivedEncoder[FieldType[K, L] :+: R] = {
    case Inl(h) => Fmt.MMap(Map(Fmt.fromString(witK.value.name) -> encodeL.value(h)))
    case Inr(t) => encodeR.value.applyToMap(t)
  }
}
