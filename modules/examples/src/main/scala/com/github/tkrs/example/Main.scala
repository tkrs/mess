package com.github.tkrs.example

import mess._
import mess.ast.MsgPack
import org.msgpack.core.MessagePack._

final case class Foo(age: Int, name: String)

final case class Bar(foos: List[Foo])

object Main extends App {

  val bar = Bar(Foo(2305, "Archimedes") :: Foo(411, "Johannes Kepler") :: Nil)

  val packed = MsgPack.pack(Encoder[Bar].apply(bar), DEFAULT_PACKER_CONFIG)

  val unpacked = Decoder[Bar].apply(MsgPack.unpack(packed, DEFAULT_UNPACKER_CONFIG))

  assert(Right(bar) == unpacked)
}
