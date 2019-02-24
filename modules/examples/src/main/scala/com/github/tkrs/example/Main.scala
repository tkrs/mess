package com.github.tkrs.example

import mess._
import mess.ast.MsgPack
import mess.codec.generic._
import org.msgpack.core.MessagePack._
import shapeless.Witness

final case class Foo(age: Int, name: String)
object Foo {
  type Name = Witness.`'name`.T

  implicit val encodeName =
    Encoder.instance[Name](n => MsgPack.fromString(n.name.toUpperCase()))
  implicit val decodeName =
    KeyDecoder.instance[Name](n => n.name.toUpperCase())

  implicit val encodeFoo: Encoder[Foo] = derivedEncoder[Foo]
  implicit val decodeFoo: Decoder[Foo] = derivedDecoder[Foo]
}

final case class Bar(foos: List[Foo])
object Bar {
  implicit val encodeBar: Encoder[Bar] = derivedEncoder[Bar]
  implicit val decodeBar: Decoder[Bar] = derivedDecoder[Bar]
}

object Main extends App {

  val bar = Bar(Foo(2305, "Archimedes") :: Foo(411, "Johannes Kepler") :: Nil)

  val encoded = Encoder[Bar].apply(bar)
  println(encoded)

  val packed = MsgPack.pack(encoded, DEFAULT_PACKER_CONFIG)

  val unpacked = MsgPack.unpack(packed, DEFAULT_UNPACKER_CONFIG)
  println(unpacked)

  val decoded = Decoder[Bar].apply(unpacked)

  assert(Right(bar) == decoded)
}
