package mess.codec

import shapeless._
import shapeless.labelled._

private[codec] trait DerivedDecoder1 extends DerivedDecoder2 { self: DerivedDecoderOps =>
  implicit final def decodeGen[A, R](implicit
    gen: LabelledGeneric.Aux[A, R],
    decodeR: Lazy[DerivedDecoder[R]]
  ): DerivedDecoder[A] = decodeGenInternal(gen, decodeR.value)
}

private[codec] trait DerivedDecoder2 extends DerivedDecoder3 { self: DerivedDecoderOps =>
  implicit final val decodeHNil: DerivedDecoder[HNil] = _ => Right(HNil)

  implicit final def decodeLabelledHList[K <: Symbol, H, T <: HList](implicit
    witK: Witness.Aux[K],
    decodeH: Decoder[H],
    decodeT: Lazy[DerivedDecoder[T]]
  ): DerivedDecoder[FieldType[K, H] :: T] = self.decodeLabelledHListInternal(witK, decodeH, decodeT.value)
}

private[codec] trait DerivedDecoder3 { self: DerivedDecoderOps =>
  implicit final val decodeCNil: DerivedDecoder[CNil] =
    m => Left(TypeMismatchError("Sum", m))

  implicit final def decodeLabelledCoproduct[K <: Symbol, L, R <: Coproduct](implicit
    witK: Witness.Aux[K],
    decodeL: Lazy[DerivedDecoder[L]],
    decodeR: Lazy[DerivedDecoder[R]]
  ): DerivedDecoder[FieldType[K, L] :+: R] = decodeLabelledCoproductInternal(witK, decodeL.value, decodeR.value)
}
