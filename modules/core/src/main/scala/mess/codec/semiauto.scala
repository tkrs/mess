package mess.codec

import shapeless.Lazy

object semiauto {
  def derivedEncoder[A](implicit A: Lazy[DerivedEncoder[A]]): Encoder.AsMap[A] = A.value
  def derivedDecoder[A](implicit A: Lazy[DerivedDecoder[A]]): Decoder[A]       = A.value
}
