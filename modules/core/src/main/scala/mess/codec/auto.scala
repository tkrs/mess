package mess.codec

import shapeless.Lazy

object auto {
  implicit def autoDerivedEncoder[A](implicit A: Lazy[DerivedEncoder[A]]): Encoder[A] = A.value
  implicit def autoDerivedDecoder[A](implicit A: Lazy[DerivedDecoder[A]]): Decoder[A] = A.value
}
