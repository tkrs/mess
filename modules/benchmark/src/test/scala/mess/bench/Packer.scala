package mess.bench

import mess.Encoder
import mess.ast.MsgPack
import mess.codec.generic._
import org.msgpack.core.MessageBufferPacker
import org.openjdk.jmh.annotations._

trait Packer {

  @inline private def encode[A](a: A, p: MessageBufferPacker)(implicit A: Encoder[A]): Array[Byte] = {
    A(a).pack(p)
    val r = p.toByteArray
    p.clear()
    r
  }

  private[this] val decodeLong10CC: Encoder[models.Long10] = derivedEncoder[models.Long10]
  private[this] val decodeLong30CC: Encoder[models.Long30] = derivedEncoder[models.Long30]
  private[this] val decodeLong60CC: Encoder[models.Long60] = derivedEncoder[models.Long60]

  @Benchmark
  def encodeUInt32(data: States.PackData): Array[Byte] = {
    encode(data.UInt32, data.packer)
  }

  @Benchmark
  def encodeUInt64(data: States.PackData): Array[Byte] = {
    encode(data.UInt64, data.packer)
  }

  @Benchmark
  def encodeStr16(data: States.PackData): Array[Byte] = {
    encode(data.Str16, data.packer)
  }

  @Benchmark
  def encodeStr32(data: States.PackData): Array[Byte] = {
    encode(data.Str32, data.packer)
  }

  @Benchmark
  def encodeLong10(data: States.PackData): Array[Byte] = {
    encode(data.Long10CC, data.packer)(decodeLong10CC)
  }

  @Benchmark
  def encodeLong30(data: States.PackData): Array[Byte] = {
    encode(data.Long30CC, data.packer)(decodeLong30CC)
  }

  @Benchmark
  def encodeLong60(data: States.PackData): Array[Byte] = {
    encode(data.Long60CC, data.packer)(decodeLong60CC)
  }
}

trait ToAst {

  private[this] val decodeLong10CC: Encoder[models.Long10] = derivedEncoder[models.Long10]
  private[this] val decodeLong30CC: Encoder[models.Long30] = derivedEncoder[models.Long30]
  private[this] val decodeLong60CC: Encoder[models.Long60] = derivedEncoder[models.Long60]

  @Benchmark
  def packLong10(data: States.PackData): MsgPack = {
    decodeLong10CC.apply(data.Long10CC)
  }

  @Benchmark
  def packLong30(data: States.PackData): MsgPack = {
    decodeLong30CC.apply(data.Long30CC)
  }

  @Benchmark
  def packLong60(data: States.PackData): MsgPack = {
    decodeLong60CC.apply(data.Long60CC)
  }
}
