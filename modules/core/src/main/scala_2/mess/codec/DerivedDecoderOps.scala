package mess.codec

import mess.Fmt
import shapeless._
import shapeless.labelled._

private[codec] trait DerivedDecoderOps {
  @inline final def decodeGenInternal[A, R](gen: LabelledGeneric.Aux[A, R],
                                            decodeR: DerivedDecoder[R]
  ): DerivedDecoder[A] =
    a =>
      decodeR(a) match {
        case Right(v) => Right(gen.from(v))
        case Left(e)  => Left(e)
      }

  @inline final def decodeLabelledHListInternal[K <: Symbol, H, T <: HList](
    witK: Witness.Aux[K],
    decodeH: Decoder[H],
    decodeT: DerivedDecoder[T]
  ): DerivedDecoder[FieldType[K, H] :: T] = {
    case m @ (a: Fmt.MMap) =>
      decodeT(m) match {
        case Right(t) =>
          val v = a.get(Fmt.MString(witK.value.name)).getOrElse(Fmt.MNil)
          decodeH(v) match {
            case Right(h) => Right(field[K](h) :: t)
            case Left(e)  => Left(e)
          }
        case Left(e) => Left(e)
      }
    case m => Left(TypeMismatchError("Product", m))
  }

  @inline final def decodeLabelledCoproductInternal[K <: Symbol, L, R <: Coproduct](implicit
    witK: Witness.Aux[K],
    decodeL: DerivedDecoder[L],
    decodeR: DerivedDecoder[R]
  ): DerivedDecoder[FieldType[K, L] :+: R] = {
    case m @ (a: Fmt.MMap) =>
      val v = a.get(Fmt.fromString(witK.value.name)).getOrElse(Fmt.MNil)
      decodeL.map(v => Inl(field[K](v))).apply(v) match {
        case r @ Right(_) => r
        case Left(_)      => decodeR.map(vv => Inr(vv)).apply(m)
      }
    case m => Left(TypeMismatchError("Sum", m))
  }
}
