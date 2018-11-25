package mess.bench

import java.util.concurrent.TimeUnit

import mess.ast.MsgPack
import mess.codec.generic._
import mess.{Decoder, Encoder}
import org.msgpack.core.MessagePack
import org.openjdk.jmh.annotations._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@OutputTimeUnit(TimeUnit.SECONDS)
@Fork(
  value = 2,
  jvmArgs = Array(
    "-server",
    "-Xms2g",
    "-Xmx2g",
    "-XX:NewSize=1g",
    "-XX:MaxNewSize=1g",
    "-XX:InitialCodeCacheSize=512m",
    "-XX:ReservedCodeCacheSize=512m",
    "-XX:+UseG1GC",
    "-XX:-UseBiasedLocking",
    "-XX:+AlwaysPreTouch"
  )
)
abstract class Bench
object Bench {

  def toBytes[A](a: A)(implicit encodeA: Encoder[A]): Array[Byte] = {
    val buffer = MessagePack.DEFAULT_PACKER_CONFIG.newBufferPacker()
    try { encodeA.apply(a).pack(buffer); buffer.toByteArray } finally buffer.close()
  }
  def fromBytes[A](bytes: Array[Byte])(implicit decodeA: Decoder[A]): A = {
    decodeA.apply(MsgPack.unpack(bytes, MessagePack.DEFAULT_UNPACKER_CONFIG)) match {
      case Right(v) => v; case Left(e) => throw e
    }
  }
}

final case class Foo(
    hoge: Int,
    bar: Long,
    baz: String,
    yoda: Boolean,
    quux: Double
)
object Foo {
  implicit val decodeFoo: Decoder[Foo] = derivedDecoder[Foo]
  implicit val encodeFoo: Encoder[Foo] = derivedEncoder[Foo]

  def next(): Foo = Foo(
    9843720,
    97693437922040L,
    "abcdefghijklmnopqrstmvwxyz",
    true,
    0.2387430
  )
}

class GenericBench extends Bench {

  private[this] val foo      = Foo.next()
  private[this] val fooBytes = Bench.toBytes(foo)

  @Benchmark
  def decodeFoo(): Decoder.Result[Foo] = {
    Decoder[Foo].apply(MsgPack.unpack(fooBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))
  }

  @Benchmark
  def encodeFoo(): Array[Byte] = {
    MsgPack.pack(Encoder[Foo].apply(foo), MessagePack.DEFAULT_PACKER_CONFIG)
  }
}

class ContainerBench extends Bench {

  @Param(Array("5", "10", "50", "100", "1000"))
  var size: Int = _

  private[this] var map: Map[Long, Double] = _
  private[this] var mapBytes: Array[Byte]  = _
  private[this] var seq: Seq[Long]         = _
  private[this] var seqBytes: Array[Byte]  = _
  private[this] var list: List[Long]       = _
  private[this] var vec: Vector[Long]      = _
  private[this] var set: Set[Long]         = _

  @Setup
  def setup(): Unit = {
    map = (1 to size).map(a => a.toLong -> a * 0.12).toMap
    mapBytes = Bench.toBytes(map)
    seq = (1L to size.toLong).toSeq
    seqBytes = Bench.toBytes(seq)
    list = seq.toList
    vec = seq.toVector
    set = seq.toSet
  }

  @Benchmark
  def decodeMap(): Decoder.Result[Map[Long, Double]] = {
    Decoder[Map[Long, Double]].apply(MsgPack.unpack(mapBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))
  }

  @Benchmark
  def encodeMap(): Array[Byte] = {
    MsgPack.pack(Encoder[Map[Long, Double]].apply(map), MessagePack.DEFAULT_PACKER_CONFIG)
  }

  @Benchmark
  def decodeSeq(): Decoder.Result[Seq[Long]] = {
    Decoder[Seq[Long]].apply(MsgPack.unpack(seqBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))
  }

  @Benchmark
  def encodeSeq(): Array[Byte] = {
    MsgPack.pack(Encoder[Seq[Long]].apply(seq), MessagePack.DEFAULT_PACKER_CONFIG)
  }

  @Benchmark
  def decodeList(): Decoder.Result[List[Long]] = {
    Decoder[List[Long]].apply(MsgPack.unpack(seqBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))
  }

  @Benchmark
  def encodeList(): Array[Byte] = {
    MsgPack.pack(Encoder[List[Long]].apply(list), MessagePack.DEFAULT_PACKER_CONFIG)
  }

  @Benchmark
  def decodeVector(): Decoder.Result[Vector[Long]] = {
    Decoder[Vector[Long]].apply(MsgPack.unpack(seqBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))
  }

  @Benchmark
  def encodeVector(): Array[Byte] = {
    MsgPack.pack(Encoder[Vector[Long]].apply(vec), MessagePack.DEFAULT_PACKER_CONFIG)
  }

  @Benchmark
  def decodeSet(): Decoder.Result[Set[Long]] = {
    Decoder[Set[Long]].apply(MsgPack.unpack(seqBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))
  }

  @Benchmark
  def encodeSet(): Array[Byte] = {
    MsgPack.pack(Encoder[Set[Long]].apply(set), MessagePack.DEFAULT_PACKER_CONFIG)
  }
}
