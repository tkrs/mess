package com.github.tkrs.examples

import mess.Fmt
import mess.codec.Decoder
import mess.codec.Encoder

object Main extends App {
  val bar = Bar(Foo(2305, "Archimedes") :: Foo(411, "Johannes Kepler") :: Nil)

  val packed = Fmt.pack(Encoder[Bar].apply(bar))

  val unpacked = Decoder[Bar].apply(Fmt.unpack(packed))

  assert(Right(bar) == unpacked)
}
