package mess.codec

private[codec] trait SemiautoOps {
  def derivedEncoder[A](implicit A: => DerivedEncoder[A]): Encoder.AsMap[A] = A
  def derivedDecoder[A](implicit A: => DerivedDecoder[A]): Decoder[A]       = A
}
