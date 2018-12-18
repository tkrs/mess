package mess.codec.generic

import export._
import mess.{Decoder, TypeMismatchError}
import mess.ast.MsgPack
import shapeless._
import shapeless.labelled.{FieldType, field}

trait DerivedDecoder[A] extends Decoder[A]

@exports
object DerivedDecoder extends LowPriorityDerivedDecoder

private[mess] trait LowPriorityDerivedDecoder {

  implicit final val decodeHNil: DerivedDecoder[HNil] =
    new DerivedDecoder[HNil] {
      def apply(a: MsgPack): Decoder.Result[HNil] = Right(HNil)
    }

  implicit final def decodeLabelledHList[K <: Symbol, H, T <: HList](
      implicit
      witK: Witness.Aux[K],
      decodeH: Decoder[H],
      decodeT: DerivedDecoder[T]): DerivedDecoder[FieldType[K, H] :: T] =
    new DerivedDecoder[FieldType[K, H] :: T] {
      def apply(m: MsgPack): Decoder.Result[FieldType[K, H] :: T] = m match {
        case MsgPack.MMap(a) =>
          decodeT(m) match {
            case Right(t) =>
              val v = a.getOrElse(MsgPack.MString(witK.value.name), MsgPack.MNil)
              decodeH(v) match {
                case Right(h) => Right(field[K](h) :: t)
                case Left(e)  => Left(e)
              }
            case Left(e) => Left(e)
          }
        case _ => Left(TypeMismatchError("FieldType[K, H] :: T", m))
      }
    }

  implicit final val decodeCNil: DerivedDecoder[CNil] =
    new DerivedDecoder[CNil] {
      override def apply(m: MsgPack): Decoder.Result[CNil] =
        Left(TypeMismatchError("CNil", m))
    }

  implicit final def decodeLabelledCCons[K <: Symbol, L, R <: Coproduct](
      implicit
      witK: Witness.Aux[K],
      decodeL: Decoder[L],
      decodeR: DerivedDecoder[R]): DerivedDecoder[FieldType[K, L] :+: R] = {
    new DerivedDecoder[FieldType[K, L] :+: R] {
      override def apply(m: MsgPack): Decoder.Result[FieldType[K, L] :+: R] = m match {
        case MsgPack.MMap(a) =>
          val v = a.getOrElse(MsgPack.fromString(witK.value.name), MsgPack.MNil)
          decodeL.map(v => Inl(field[K](v))).apply(v) match {
            case r @ Right(_) => r
            case Left(_)      => decodeR.map(vv => Inr(vv)).apply(m)
          }
        case _ => Left(TypeMismatchError("FieldType[K, L] :+: R", m))
      }
    }
  }

  implicit final def decodeGen[A, R](implicit
                                     gen: LabelledGeneric.Aux[A, R],
                                     decodeR: Lazy[DerivedDecoder[R]]): DerivedDecoder[A] =
    new DerivedDecoder[A] {
      def apply(a: MsgPack): Decoder.Result[A] = decodeR.value(a) match {
        case Right(v) => Right(gen.from(v))
        case Left(e)  => Left(e)
      }
    }
}
