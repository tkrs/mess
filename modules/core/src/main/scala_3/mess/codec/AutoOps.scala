package mess.codec

import scala.deriving.*

private[codec] trait AutoOps:
  inline given autoDerivedEncoder[A](using A: Mirror.Of[A]): Encoder.AsMap[A] = Encoder.AsMap.derived[A]
  inline given autoDerivedDecoder[A](using A: Mirror.Of[A]): Decoder[A]       = Decoder.derived[A]
