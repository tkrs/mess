package mess.examples

import mess._
import org.msgpack.core.MessagePack._

final case class Foo(age: Int, name: String)
object Foo {
  implicit val encodeFoo: Encoder[Foo] = Codec.derivedEncoder[Foo]
  implicit val decodeFoo: Decoder[Foo] = Codec.derivedDecoder[Foo]
}

final case class Bar(foos: List[Foo])
object Bar {
  implicit val encodeBar: Encoder[Bar] = Codec.derivedEncoder[Bar]
  implicit val decodeBar: Decoder[Bar] = Codec.derivedDecoder[Bar]
}

object Main extends App {

  val bar = Bar(Foo(2305, "Archimedes") :: Foo(411, "Johannes Kepler") :: Nil)

  val bytes = {
    val packer = DEFAULT_PACKER_CONFIG.newBufferPacker()
    try {
      Codec.serialize(Encoder[Bar].apply(bar), packer)
      packer.toByteArray
    } finally {
      packer.close()
    }
  }

  val unpacker = DEFAULT_UNPACKER_CONFIG.newUnpacker(bytes)

  val bar2 = Decoder[Bar].apply(Codec.deserialize(unpacker)).right.get

  assert(bar == bar2)
}
