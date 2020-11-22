package mess.codec

import shapeless._
import shapeless.labelled._

private[codec] trait DerivedDecoder1 extends DerivedDecoder2 { self: DerivedDecoderOps =>
  implicit final def decodeGen[A, R](implicit
    gen: LabelledGeneric.Aux[A, R],
    decodeR: => DerivedDecoder[R]
  ): DerivedDecoder[A] = decodeGenInternal(gen, decodeR)
}

private[codec] trait DerivedDecoder2 extends DerivedDecoder3 { self: DerivedDecoderOps =>
  implicit final val decodeHNil: DerivedDecoder[HNil] = _ => Right(HNil)

  implicit final def decodeLabelledHList[K <: Symbol, H, T <: HList](implicit
    witK: Witness.Aux[K],
    decodeH: Decoder[H],
    decodeT: => DerivedDecoder[T]
  ): DerivedDecoder[FieldType[K, H] :: T] = self.decodeLabelledHListInternal(witK, decodeH, decodeT)
}

private[codec] trait DerivedDecoder3 { self: DerivedDecoderOps =>
  implicit final val decodeCNil: DerivedDecoder[CNil] =
    m => Left(TypeMismatchError("CNil", m))

  implicit final def decodeLabelledCoproduct[K <: Symbol, L, R <: Coproduct](implicit
    witK: Witness.Aux[K],
    decodeL: => DerivedDecoder[L],
    decodeR: => DerivedDecoder[R]
  ): DerivedDecoder[FieldType[K, L] :+: R] = decodeLabelledCoproductInternal(witK, decodeL, decodeR)
}
