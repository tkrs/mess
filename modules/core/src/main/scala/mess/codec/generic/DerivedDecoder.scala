package mess.codec.generic

import export._
import mess.Decoder
import mess.ast.MsgPack
import shapeless._
import shapeless.labelled.{FieldType, field}

trait DerivedDecoder[A] extends Decoder[A]

@exports
object DerivedDecoder extends LowPriorityDerivedDecoder

trait LowPriorityDerivedDecoder {

  implicit final val decodeHNil: DerivedDecoder[HNil] =
    new DerivedDecoder[HNil] {
      def apply(a: MsgPack): Either[Throwable, HNil] = Right(HNil)
    }

  implicit final def decodeLabelledHList[K <: Symbol, H, T <: HList](
      implicit
      K: Witness.Aux[K],
      H: Decoder[H],
      T: DerivedDecoder[T]): DerivedDecoder[FieldType[K, H] :: T] =
    new DerivedDecoder[FieldType[K, H] :: T] {
      def apply(m: MsgPack): Either[Throwable, FieldType[K, H] :: T] = m match {
        case MsgPack.MMap(a) =>
          T(m) match {
            case Right(t) =>
              val v = a.get(MsgPack.MString(K.value.name))
              H(if (v == null) MsgPack.MNil else v) match {
                case Right(h) => Right(field[K](h) :: t)
                case Left(e)  => Left(e)
              }
            case Left(e) => Left(e)
          }
        case _ => Left(new IllegalArgumentException(s"$m"))
      }
    }

  implicit final val decodeCNil: DerivedDecoder[CNil] =
    new DerivedDecoder[CNil] {
      override def apply(m: MsgPack): Either[Throwable, CNil] =
        Left(new IllegalStateException(s"$m cannot decoding to CNil"))
    }

  implicit final def decodeCoproductCons[L, R <: Coproduct](implicit
                                                            L: Decoder[L],
                                                            R: DerivedDecoder[R]): DerivedDecoder[L :+: R] = {
    new DerivedDecoder[L :+: R] {
      override def apply(m: MsgPack): Either[Throwable, L :+: R] = {
        L.map(Inl(_)).apply(m) match {
          case r @ Right(_) => r
          case Left(_)      => R.map(Inr(_)).apply(m)
        }
      }
    }
  }

  implicit final def decodeLabelledCoproductCons[K <: Symbol, L, R <: Coproduct](
      implicit
      K: Witness.Aux[K],
      L: Decoder[L],
      R: DerivedDecoder[R]): DerivedDecoder[FieldType[K, L] :+: R] = {
    new DerivedDecoder[FieldType[K, L] :+: R] {
      override def apply(m: MsgPack): Either[Throwable, FieldType[K, L] :+: R] = m match {
        case MsgPack.MMap(a) =>
          val v  = a.get(MsgPack.MString(K.value.name))
          val vv = if (v == null) MsgPack.MNil else v
          L.map(v => Inl(field[K](v))).apply(vv) match {
            case r @ Right(_) => r
            case Left(_)      => R.map(vv => Inr(vv)).apply(m)
          }
        case _ => Left(new IllegalArgumentException(s"$m"))
      }
    }
  }

  implicit final def decodeGen[A, R](implicit
                                     gen: LabelledGeneric.Aux[A, R],
                                     R: Lazy[DerivedDecoder[R]]): DerivedDecoder[A] =
    new DerivedDecoder[A] {
      def apply(a: MsgPack): Either[Throwable, A] = R.value(a) match {
        case Right(v) => Right(gen.from(v))
        case Left(e)  => Left(e)
      }
    }
}
