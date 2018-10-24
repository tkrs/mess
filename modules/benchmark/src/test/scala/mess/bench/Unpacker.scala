package mess.bench

import java.nio.ByteBuffer

import mess.Decoder
import mess.ast.MsgPack
import mess.codec.generic._
import org.msgpack.core.MessagePack
import org.openjdk.jmh.annotations._

trait Unpacker {

  @inline private def decode[A](src: ByteBuffer)(implicit A: Decoder[A]): A = {
    val p   = MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(src)
    val dst = MsgPack.unpack(p)
    val r   = A(dst).right.get
    p.close()
    r
  }

  private[this] implicit val decodeLong10CC: Decoder[models.Long10] = derivedDecoder[models.Long10]
  private[this] implicit val decodeLong30CC: Decoder[models.Long30] = derivedDecoder[models.Long30]
  private[this] implicit val decodeLong60CC: Decoder[models.Long60] = derivedDecoder[models.Long60]

  @Benchmark
  def decodeUInt32(data: States.UnpackData): Long = {
    decode[Long](data.uInt32)
  }

  @Benchmark
  def decodeUInt64(data: States.UnpackData): BigInt = {
    decode[BigInt](data.uInt64)
  }

  @Benchmark
  def decodeStr16(data: States.UnpackData): String = {
    decode[String](data.str16V)
  }

  @Benchmark
  def decodeStr32(data: States.UnpackData): String = {
    decode[String](data.str32V)
  }

  @Benchmark
  def decodeLong10(data: States.UnpackData): models.Long10 = {
    decode[models.Long10](data.long10CC)
  }

  @Benchmark
  def decodeLong30(data: States.UnpackData): models.Long30 = {
    decode[models.Long30](data.long30CC)
  }

  @Benchmark
  def decodeLong60(data: States.UnpackData): models.Long60 = {
    decode[models.Long60](data.long60CC)
  }
}

trait FromAst {

  private[this] implicit val decodeLong10CC: Decoder[models.Long10] = derivedDecoder[models.Long10]
  private[this] implicit val decodeLong30CC: Decoder[models.Long30] = derivedDecoder[models.Long30]
  private[this] implicit val decodeLong60CC: Decoder[models.Long60] = derivedDecoder[models.Long60]

  @Benchmark
  def decodeLong10(data: States.UnpackData): models.Long10 = {
    decodeLong10CC(data.long10CCP).right.get
  }

  @Benchmark
  def decodeLong30(data: States.UnpackData): models.Long30 = {
    decodeLong30CC(data.long30CCP).right.get
  }

  @Benchmark
  def decodeLong60(data: States.UnpackData): models.Long60 = {
    decodeLong60CC(data.long60CCP).right.get
  }
}
