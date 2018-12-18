package mess.codec.generic

import export._
import mess.Decoder.Result
import mess.{Decoder, TypeMismatchError}
import mess.ast.MsgPack
import shapeless._
import shapeless.labelled.{FieldType, field}

trait DerivedDecoder[A] extends Decoder[A] {

  def apply(m: MsgPack): Decoder.Result[A] =
    m.asMap match {
      case Some(a) => decodeMap(a)
      case _       => Left(TypeMismatchError("FieldType[K, H] :: T", m))
    }

  def decodeMap(m: Map[MsgPack, MsgPack]): Decoder.Result[A]
}

@exports
object DerivedDecoder extends LowPriorityDerivedDecoder

private[mess] trait LowPriorityDerivedDecoder {

  implicit final val decodeHNil: DerivedDecoder[HNil] =
    new DerivedDecoder[HNil] {
      override def apply(a: MsgPack): Decoder.Result[HNil]  = Right(HNil)
      def decodeMap(m: Map[MsgPack, MsgPack]): Result[HNil] = Right(HNil)
    }

  implicit final def decodeLabelledHList[K <: Symbol, H, T <: HList](
      implicit
      witK: Witness.Aux[K],
      decodeH: Decoder[H],
      decodeT: DerivedDecoder[T]): DerivedDecoder[FieldType[K, H] :: T] =
    new DerivedDecoder[FieldType[K, H] :: T] {
      def decodeMap(m: Map[MsgPack, MsgPack]): Result[FieldType[K, H] :: T] = {
        decodeT.decodeMap(m) match {
          case Right(t) =>
            val v = m.getOrElse(MsgPack.MString(witK.value.name), MsgPack.MNil)
            decodeH(v) match {
              case Right(h) => Right(field[K](h) :: t)
              case Left(e)  => Left(e)
            }
          case Left(e) => Left(e)
        }
      }
    }

  implicit final val decodeCNil: DerivedDecoder[CNil] =
    new DerivedDecoder[CNil] {
      override def apply(m: MsgPack): Result[CNil]          = Left(TypeMismatchError("CNil", m))
      def decodeMap(m: Map[MsgPack, MsgPack]): Result[CNil] = sys.error("Should be not invoked")
    }

  implicit final def decodeLabelledCCons[K <: Symbol, L, R <: Coproduct](
      implicit
      witK: Witness.Aux[K],
      decodeL: Decoder[L],
      decodeR: DerivedDecoder[R]): DerivedDecoder[FieldType[K, L] :+: R] = {
    new DerivedDecoder[FieldType[K, L] :+: R] {
      override def apply(m: MsgPack): Result[FieldType[K, L] :+: R] = m match {
        case MsgPack.MMap(a) =>
          decodeMap(a) match {
            case r @ Right(_) => r
            case Left(_)      => decodeR.map(Inr.apply).apply(m)
          }
        case _ => Left(TypeMismatchError("FieldType[K, L] :+: R", m))
      }
      def decodeMap(m: Map[MsgPack, MsgPack]): Result[FieldType[K, L] :+: R] = {
        val v = m.getOrElse(MsgPack.fromString(witK.value.name), MsgPack.MNil)
        decodeL.map(l => Inl(field[K](l))).apply(v)
      }
    }
  }

  implicit final def decodeGen[A, R](implicit
                                     gen: LabelledGeneric.Aux[A, R],
                                     decodeR: Lazy[DerivedDecoder[R]]): DerivedDecoder[A] =
    new DerivedDecoder[A] {
      def decodeMap(m: Map[MsgPack, MsgPack]): Result[A] =
        decodeR.value.decodeMap(m) match {
          case Right(v) => Right(gen.from(v))
          case Left(e)  => Left(e)
        }
    }
}
