package mess.bench

import java.nio.ByteBuffer

import mess.{Codec, Decoder}
import mess.derived.derivedCodecs._
import org.msgpack.core.MessagePack
import org.openjdk.jmh.annotations._

trait UnpackerBench {

  @inline private def decode[A: Decoder](src: ByteBuffer): A = {
    val p   = MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(src)
    val dst = Codec.deserialize(p)
    val r   = Decoder[A].apply(dst).right.get
    p.close()
    r
  }

  private[this] val decodeLong10CC: Decoder[models.Long10] =
    Decoder[models.Long10]
  private[this] val decodeLong30CC: Decoder[models.Long30] =
    Decoder[models.Long30]
  private[this] val decodeLong60CC: Decoder[models.Long60] =
    Decoder[models.Long60]
  @Benchmark
  def decodeLong10(data: States.UnpackData): models.Long10 = {
    decode[models.Long10](data.long10CC)(decodeLong10CC)
  }

  @Benchmark
  def decodeLong30(data: States.UnpackData): models.Long30 = {
    decode[models.Long30](data.long30CC)(decodeLong30CC)
  }

  @Benchmark
  def decodeLong60(data: States.UnpackData): models.Long60 = {
    decode[models.Long60](data.long60CC)(decodeLong60CC)
  }
}
