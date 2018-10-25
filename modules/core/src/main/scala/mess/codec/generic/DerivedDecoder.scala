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
      K: Witness.Aux[K],
      H: Decoder[H],
      T: DerivedDecoder[T]): DerivedDecoder[FieldType[K, H] :: T] =
    new DerivedDecoder[FieldType[K, H] :: T] {
      def apply(m: MsgPack): Decoder.Result[FieldType[K, H] :: T] = m match {
        case MsgPack.MMap(a) =>
          T(m) match {
            case Right(t) =>
              val v = a.get(MsgPack.MString(K.value.name))
              H(v.getOrElse(MsgPack.MNil)) match {
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
      K: Witness.Aux[K],
      L: Decoder[L],
      R: DerivedDecoder[R]): DerivedDecoder[FieldType[K, L] :+: R] = {
    new DerivedDecoder[FieldType[K, L] :+: R] {
      override def apply(m: MsgPack): Decoder.Result[FieldType[K, L] :+: R] = m match {
        case MsgPack.MMap(a) =>
          val v  = a.get(MsgPack.MString(K.value.name))
          val vv = v.getOrElse(MsgPack.MNil)
          L.map(v => Inl(field[K](v))).apply(vv) match {
            case r @ Right(_) => r
            case Left(_)      => R.map(vv => Inr(vv)).apply(m)
          }
        case _ => Left(TypeMismatchError("FieldType[K, L] :+: R", m))
      }
    }
  }

  implicit final def decodeGen[A, R](implicit
                                     gen: LabelledGeneric.Aux[A, R],
                                     R: Lazy[DerivedDecoder[R]]): DerivedDecoder[A] =
    new DerivedDecoder[A] {
      def apply(a: MsgPack): Decoder.Result[A] = R.value(a) match {
        case Right(v) => Right(gen.from(v))
        case Left(e)  => Left(e)
      }
    }
}
