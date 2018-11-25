package mess.examples

import mess._
import mess.ast.MsgPack
import mess.codec.generic._
import org.msgpack.core.MessagePack._

final case class Foo(age: Int, name: String)
object Foo {
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

  val packed = MsgPack.pack(Encoder[Bar].apply(bar), DEFAULT_PACKER_CONFIG)

  val unpacked = Decoder[Bar].apply(MsgPack.unpack(packed, DEFAULT_UNPACKER_CONFIG)).right.get

  assert(bar == unpacked)
}
