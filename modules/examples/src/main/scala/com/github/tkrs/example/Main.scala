package com.github.tkrs.example

import mess._
import org.msgpack.core.MessagePack._

final case class Foo(age: Int, name: String)

final case class Bar(foos: List[Foo])

object Main extends App {

  val bar = Bar(Foo(2305, "Archimedes") :: Foo(411, "Johannes Kepler") :: Nil)

  val packed = Fmt.pack(Encoder[Bar].apply(bar), DEFAULT_PACKER_CONFIG)

  val unpacked = Decoder[Bar].apply(Fmt.unpack(packed, DEFAULT_UNPACKER_CONFIG))

  assert(Right(bar) == unpacked)
}
