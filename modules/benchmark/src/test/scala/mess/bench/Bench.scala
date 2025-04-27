package mess.bench

import java.util.concurrent.TimeUnit

import mess.Fmt
import mess.codec.Decoder
import mess.codec.DecodingFailure
import mess.codec.Encoder
import org.msgpack.core.MessagePack
import org.openjdk.jmh.annotations.*

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 5)
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
    try {
      encodeA.apply(a).pack(buffer)
      buffer.toByteArray
    } finally buffer.close()
  }

  def fromBytes[A](bytes: Array[Byte])(implicit decodeA: Decoder[A]): A =
    decodeA.apply(Fmt.unpack(bytes, MessagePack.DEFAULT_UNPACKER_CONFIG)) match {
      case Right(v) => v;
      case Left(e)  => throw e
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
  import mess.codec.semiauto._

  implicit val decoder: Decoder[Foo] = derivedDecoder[Foo]
  implicit val encoder: Encoder[Foo] = derivedEncoder[Foo]

  def next(): Foo =
    Foo(
      9843720,
      97693437922040L,
      "abcdefghijklmnopqrstmvwxyz",
      yoda = true,
      0.2387430
    )
}

class GenericBench extends Bench {
  private val foo      = Foo.next()
  private val fooBytes = Bench.toBytes(foo)

  @Benchmark
  def decodeFoo(): Either[DecodingFailure, Foo] =
    Decoder[Foo].apply(Fmt.unpack(fooBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))

  @Benchmark
  def encodeFoo(): Array[Byte] =
    Fmt.pack(Encoder[Foo].apply(foo), MessagePack.DEFAULT_PACKER_CONFIG)
}

class ContainerBench extends Bench {
  @Param(Array("5", "10", "50", "100", "1000"))
  var size: Int = _

  private var map: Map[Long, Double] = _
  private var mapBytes: Array[Byte]  = _
  private var seq: Seq[Long]         = _
  private var seqBytes: Array[Byte]  = _
  private var list: List[Long]       = _
  private var vec: Vector[Long]      = _
  private var set: Set[Long]         = _

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
  def decodeMap(): Either[DecodingFailure, Map[Long, Double]] =
    Decoder[Map[Long, Double]].apply(Fmt.unpack(mapBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))

  @Benchmark
  def encodeMap(): Array[Byte] =
    Fmt.pack(Encoder[Map[Long, Double]].apply(map), MessagePack.DEFAULT_PACKER_CONFIG)

  @Benchmark
  def decodeSeq(): Either[DecodingFailure, Seq[Long]] =
    Decoder[Seq[Long]].apply(Fmt.unpack(seqBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))

  @Benchmark
  def encodeSeq(): Array[Byte] =
    Fmt.pack(Encoder[Seq[Long]].apply(seq), MessagePack.DEFAULT_PACKER_CONFIG)

  @Benchmark
  def decodeList(): Either[DecodingFailure, List[Long]] =
    Decoder[List[Long]].apply(Fmt.unpack(seqBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))

  @Benchmark
  def encodeList(): Array[Byte] =
    Fmt.pack(Encoder[List[Long]].apply(list), MessagePack.DEFAULT_PACKER_CONFIG)

  @Benchmark
  def decodeVector(): Either[DecodingFailure, Vector[Long]] =
    Decoder[Vector[Long]].apply(Fmt.unpack(seqBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))

  @Benchmark
  def encodeVector(): Array[Byte] =
    Fmt.pack(Encoder[Vector[Long]].apply(vec), MessagePack.DEFAULT_PACKER_CONFIG)

  @Benchmark
  def decodeSet(): Either[DecodingFailure, Set[Long]] =
    Decoder[Set[Long]].apply(Fmt.unpack(seqBytes, MessagePack.DEFAULT_UNPACKER_CONFIG))

  @Benchmark
  def encodeSet(): Array[Byte] =
    Fmt.pack(Encoder[Set[Long]].apply(set), MessagePack.DEFAULT_PACKER_CONFIG)
}
