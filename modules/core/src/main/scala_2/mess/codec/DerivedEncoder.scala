package mess.codec

trait DerivedEncoder[A] extends Encoder.AsMap[A]

object DerivedEncoder extends DerivedEncoder1 with DerivedEncoderOps
