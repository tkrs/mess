package com.github.tkrs.examples

import mess.codec.Decoder
import mess.codec.Encoder
import mess.codec.semiauto._

final case class Foo(age: Int, name: String)

object Foo {
  implicit val encode: Encoder[Foo] = derivedEncoder[Foo]
  implicit val decode: Decoder[Foo] = derivedDecoder[Foo]
}
