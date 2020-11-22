package mess.codec

import shapeless.Lazy

private[codec] trait AutoOps {
  implicit def autoDerivedEncoder[A](implicit A: Lazy[DerivedEncoder[A]]): Encoder.AsMap[A] = A.value
  implicit def autoDerivedDecoder[A](implicit A: Lazy[DerivedDecoder[A]]): Decoder[A]       = A.value
}
