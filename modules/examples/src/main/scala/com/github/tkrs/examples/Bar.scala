package com.github.tkrs.examples

import mess.codec.Decoder
import mess.codec.Encoder
import mess.codec.semiauto._

final case class Bar(foos: List[Foo])

object Bar {
  implicit val encode: Encoder[Bar] = derivedEncoder[Bar]
  implicit val decode: Decoder[Bar] = derivedDecoder[Bar]
}
