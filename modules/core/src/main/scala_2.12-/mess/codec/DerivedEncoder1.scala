package mess.codec

import mess.Fmt
import shapeless._
import shapeless.labelled._

private[codec] trait DerivedEncoder1 extends DerivedEncoder2 { self: DerivedEncoderOps =>
  implicit final def encodeGen[A, R](implicit
    gen: LabelledGeneric.Aux[A, R],
    encodeR: Lazy[DerivedEncoder[R]]
  ): DerivedEncoder[A] = encodeGenInternal(gen, encodeR.value)
}

private[codec] trait DerivedEncoder2 extends DerivedEncoder3 { self: DerivedEncoderOps =>
  implicit final val encodeHNil: DerivedEncoder[HNil] =
    _ => Fmt.MMap.newBuilder

  implicit final def encodeLabelledHList[K <: Symbol, H, T <: HList](implicit
    witK: Witness.Aux[K],
    encodeK: Encoder[K],
    encodeH: Encoder[H],
    encodeT: Lazy[DerivedEncoder[T]]
  ): DerivedEncoder[FieldType[K, H] :: T] = encodeLabelledHListInternal(witK, encodeK, encodeH, encodeT.value)
}

private[codec] trait DerivedEncoder3 { self: DerivedEncoderOps =>
  implicit final val encodeCNil: DerivedEncoder[CNil] =
    _ => sys.error("Cannot encode CNil")

  implicit final def encodeLabelledCCons[K <: Symbol, L, R <: Coproduct](implicit
    witK: Witness.Aux[K],
    encodeL: Lazy[DerivedEncoder[L]],
    encodeR: Lazy[DerivedEncoder[R]]
  ): DerivedEncoder[FieldType[K, L] :+: R] = encodeLabelledCConsInternal(witK, encodeL.value, encodeR.value)
}
