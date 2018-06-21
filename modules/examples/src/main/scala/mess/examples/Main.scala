package mess.examples

import mess._
import mess.derived.codecs._
import org.msgpack.core.MessagePack._

final case class Foo(age: Int, name: String)
final case class Bar(foos: List[Foo])

object Main extends App {
  val encodeBar: Encoder[Bar] = Encoder[Bar]
  val decodeBar: Decoder[Bar] = Decoder[Bar]

  val bar = Bar(Foo(2305, "Archimedes") :: Foo(411, "Johannes Kepler") :: Nil)

  val bytes = {
    val packer = DEFAULT_PACKER_CONFIG.newBufferPacker()
    try {
      Codec.serialize(encodeBar(bar), packer)
      packer.toByteArray()
    } finally {
      packer.close()
    }
  }

  val unpacker = DEFAULT_UNPACKER_CONFIG.newUnpacker(bytes)

  val bar2 = decodeBar(Codec.deserialize(unpacker)).right.get

  assert(bar == bar2)
}
