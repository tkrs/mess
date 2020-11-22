package mess.codec

trait DerivedDecoder[A] extends Decoder[A]

object DerivedDecoder extends DerivedDecoder1 with DerivedDecoderOps
