package com.github.tkrs.examples

import mess.Fmt
import mess.codec.Decoder
import mess.codec.Encoder
import org.msgpack.core.MessagePack.DEFAULT_PACKER_CONFIG
import org.msgpack.core.MessagePack.DEFAULT_UNPACKER_CONFIG

object Main extends App {
  val bar = Bar(Foo(2305, "Archimedes") :: Foo(411, "Johannes Kepler") :: Nil)

  val packed = Fmt.pack(Encoder[Bar].apply(bar), DEFAULT_PACKER_CONFIG)

  val unpacked = Decoder[Bar].apply(Fmt.unpack(packed, DEFAULT_UNPACKER_CONFIG))

  assert(Right(bar) == unpacked)
}
