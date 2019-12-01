package mess.codec

import mess.Fmt
import shapeless._
import shapeless.labelled._

trait DerivedDecoder[A] extends Decoder[A]

object DerivedDecoder extends DerivedDecoder1

trait DerivedDecoder1 extends DerivedDecoder2 {

  implicit final def decodeGen[A, R](
    implicit
    gen: LabelledGeneric.Aux[A, R],
    decodeR: Lazy[DerivedDecoder[R]]
  ): DerivedDecoder[A] =
    a =>
      decodeR.value(a) match {
        case Right(v) => Right(gen.from(v))
        case Left(e)  => Left(e)
      }
}

trait DerivedDecoder2 extends DerivedDecoder3 {
  implicit final val decodeHNil: DerivedDecoder[HNil] = _ => Right(HNil)

  implicit final def decodeLabelledHList[K <: Symbol, H, T <: HList](
    implicit
    witK: Witness.Aux[K],
    decodeH: Decoder[H],
    decodeT: Lazy[DerivedDecoder[T]]
  ): DerivedDecoder[FieldType[K, H] :: T] = {
    case m @ (a: Fmt.MMap) =>
      decodeT.value(m) match {
        case Right(t) =>
          val v = a.get(Fmt.MString(witK.value.name)).getOrElse(Fmt.MNil)
          decodeH(v) match {
            case Right(h) => Right(field[K](h) :: t)
            case Left(e)  => Left(e)
          }
        case Left(e) => Left(e)
      }
    case m => Left(TypeMismatchError("FieldType[K, H] :: T", m))
  }
}

trait DerivedDecoder3 {

  implicit final val decodeCNil: DerivedDecoder[CNil] =
    m => Left(TypeMismatchError("CNil", m))

  implicit final def decodeLabelledCoproduct[K <: Symbol, L, R <: Coproduct](
    implicit
    witK: Witness.Aux[K],
    decodeL: Lazy[DerivedDecoder[L]],
    decodeR: Lazy[DerivedDecoder[R]]
  ): DerivedDecoder[FieldType[K, L] :+: R] = {
    case m @ (a: Fmt.MMap) =>
      val v = a.get(Fmt.fromString(witK.value.name)).getOrElse(Fmt.MNil)
      decodeL.value.map(v => Inl(field[K](v))).apply(v) match {
        case r @ Right(_) => r
        case Left(_)      => decodeR.value.map(vv => Inr(vv)).apply(m)
      }
    case m => Left(TypeMismatchError("FieldType[K, L] :+: R", m))
  }
}
