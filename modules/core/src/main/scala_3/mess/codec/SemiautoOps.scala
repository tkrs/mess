package mess.codec

import scala.deriving._

private[codec] trait SemiautoOps:
  inline def derivedEncoder[A](using A: Mirror.Of[A]): Encoder.AsMap[A] = Encoder.AsMap.derived[A]
  inline def derivedDecoder[A](using A: Mirror.Of[A]): Decoder[A]       = Decoder.derived[A]
