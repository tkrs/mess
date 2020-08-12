package com.github.tkrs.example

import mess.Fmt
import mess.codec.Decoder
import mess.codec.Encoder
import mess.codec.semiauto._
import org.msgpack.core.MessagePack._

final case class Foo(age: Int, name: String)

object Foo {
  implicit val encode: Encoder[Foo] = derivedEncoder[Foo]
  implicit val decode: Decoder[Foo] = derivedDecoder[Foo]
}

final case class Bar(foos: List[Foo])

object Bar {
  implicit val encode: Encoder[Bar] = derivedEncoder[Bar]
  implicit val decode: Decoder[Bar] = derivedDecoder[Bar]
}

object Main extends App {
  val bar = Bar(Foo(2305, "Archimedes") :: Foo(411, "Johannes Kepler") :: Nil)

  val packed = Fmt.pack(Encoder[Bar].apply(bar), DEFAULT_PACKER_CONFIG)

  val unpacked = Decoder[Bar].apply(Fmt.unpack(packed, DEFAULT_UNPACKER_CONFIG))

  assert(Right(bar) == unpacked)
}
