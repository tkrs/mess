package mess.codec

private[codec] trait AutoOps {
  implicit def autoDerivedEncoder[A](implicit A: => DerivedEncoder[A]): Encoder.AsMap[A] = A
  implicit def autoDerivedDecoder[A](implicit A: => DerivedDecoder[A]): Decoder[A]       = A
}
