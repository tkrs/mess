package mess.codec

import mess.Fmt
import shapeless._
import shapeless.labelled._

private[codec] trait DerivedEncoderOps {

  @inline final def encodeGenInternal[A, R](gen: LabelledGeneric.Aux[A, R],
                                            encodeR: => DerivedEncoder[R]
  ): DerivedEncoder[A] =
    a => encodeR.applyToMap(gen.to(a))

  @inline final def encodeLabelledHListInternal[K <: Symbol, H, T <: HList](
    witK: Witness.Aux[K],
    encodeK: Encoder[K],
    encodeH: Encoder[H],
    encodeT: DerivedEncoder[T]
  ): DerivedEncoder[FieldType[K, H] :: T] =
    a => encodeT.applyToMap(a.tail) + (encodeK(witK.value) -> encodeH(a.head))

  @inline final def encodeLabelledCConsInternal[K <: Symbol, L, R <: Coproduct](
    witK: Witness.Aux[K],
    encodeL: => DerivedEncoder[L],
    encodeR: => DerivedEncoder[R]
  ): DerivedEncoder[FieldType[K, L] :+: R] = {
    case Inl(h) => Fmt.MMap(Map(Fmt.fromString(witK.value.name) -> encodeL(h)))
    case Inr(t) => encodeR.applyToMap(t)
  }
}
