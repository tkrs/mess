package mess.bench

import java.util.concurrent.TimeUnit

import mess.ast.MsgPack
import mess.codec.generic._
import mess.{Decoder, Encoder}
import org.msgpack.core.MessagePack
import org.openjdk.jmh.annotations._

import scala.util.Random

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
  val rnd = new Random()

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
    Bench.rnd.nextInt(),
    Bench.rnd.nextLong(),
    Bench.rnd.nextString(10),
    Bench.rnd.nextBoolean(),
    Bench.rnd.nextDouble()
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

  private[this] var seq: Seq[Long]        = _
  private[this] var seqBytes: Array[Byte] = _

  @Setup
  def setup(): Unit = {
    map = Iterator.continually(Bench.rnd.nextLong() -> Bench.rnd.nextDouble()).take(size).toMap
    mapBytes = Bench.toBytes(map)
    seq = Iterator.continually(Bench.rnd.nextLong()).take(size).toSeq
    seqBytes = Bench.toBytes(seq)
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
}
