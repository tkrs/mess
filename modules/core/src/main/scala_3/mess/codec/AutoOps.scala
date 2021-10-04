package mess.codec

import scala.deriving._

private[codec] trait AutoOps:
  implicit inline def autoDerivedEncoder[A](using A: Mirror.Of[A]): Encoder.AsMap[A] = Encoder.AsMap.derived[A]
  implicit inline def autoDerivedDecoder[A](using A: Mirror.Of[A]): Decoder[A]       = Decoder.derived[A]
