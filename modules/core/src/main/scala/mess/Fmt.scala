package mess

import java.math.BigInteger
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant

import org.msgpack.core.MessageFormat as MF
import org.msgpack.core.MessagePack
import org.msgpack.core.MessagePacker
import org.msgpack.core.MessageUnpacker

import scala.annotation.tailrec
import scala.collection.Factory
import scala.collection.Iterator
import scala.collection.immutable
import scala.collection.mutable

sealed trait Fmt extends Serializable {
  def isNil: Boolean
  def pack(buffer: MessagePacker): Unit
}

object Fmt {
  case object MUnit extends Fmt {
    def isNil: Boolean                    = false
    def pack(buffer: MessagePacker): Unit = ()
  }

  case object MNil extends Fmt {
    def isNil: Boolean = true

    def pack(buffer: MessagePacker): Unit =
      buffer.packNil()
  }

  sealed abstract class MBool(val value: Boolean) extends Fmt {
    def isNil: Boolean = false

    def pack(buffer: MessagePacker): Unit =
      buffer.packBoolean(value)
  }

  object MBool {
    def apply(value: Boolean): MBool =
      if (value) True else False

    case object True  extends MBool(value = true)
    case object False extends MBool(value = false)
  }

  sealed trait MNumber extends Fmt {
    def isNil: Boolean = false

    def asByte: Byte
    def asShort: Short
    def asInt: Int
    def asLong: Long
    def asBigInt: BigInt
    def asFloat: Float
    def asDouble: Double
  }

  final case class MByte(value: Byte) extends MNumber {
    def pack(buffer: MessagePacker): Unit =
      buffer.packByte(value)
    def asByte: Byte     = value
    def asShort: Short   = value.toShort
    def asInt: Int       = value.toInt
    def asLong: Long     = value.toLong
    def asBigInt: BigInt = BigInt(value.toInt)
    def asFloat: Float   = value.toFloat
    def asDouble: Double = value.toDouble
  }

  final case class MShort(value: Short) extends MNumber {
    def pack(buffer: MessagePacker): Unit =
      buffer.packShort(value)
    def asByte: Byte     = value.toByte
    def asShort: Short   = value
    def asInt: Int       = value.toInt
    def asLong: Long     = value.toLong
    def asBigInt: BigInt = BigInt(value.toInt)
    def asFloat: Float   = value.toFloat
    def asDouble: Double = value.toDouble
  }

  final case class MInt(value: Int) extends MNumber {
    def pack(buffer: MessagePacker): Unit =
      buffer.packInt(value)
    def asByte: Byte     = value.toByte
    def asShort: Short   = value.toShort
    def asInt: Int       = value
    def asLong: Long     = value.toLong
    def asBigInt: BigInt = BigInt(value)
    def asFloat: Float   = value.toFloat
    def asDouble: Double = value.toDouble
  }

  final case class MLong(value: Long) extends MNumber {
    def pack(buffer: MessagePacker): Unit =
      buffer.packLong(value)
    def asByte: Byte     = value.toByte
    def asShort: Short   = value.toShort
    def asInt: Int       = value.toInt
    def asLong: Long     = value
    def asBigInt: BigInt = BigInt(value)
    def asFloat: Float   = value.toFloat
    def asDouble: Double = value.toDouble
  }

  final case class MBigInt(value: BigInt) extends MNumber {
    def pack(buffer: MessagePacker): Unit =
      buffer.packBigInteger(value.bigInteger)
    def asByte: Byte     = value.toByte
    def asShort: Short   = value.toShort
    def asInt: Int       = value.toInt
    def asLong: Long     = value.toLong
    def asBigInt: BigInt = value
    def asFloat: Float   = value.toFloat
    def asDouble: Double = value.toDouble
  }

  final case class MFloat(value: Float) extends MNumber {
    def pack(buffer: MessagePacker): Unit =
      buffer.packFloat(value)
    def asByte: Byte     = value.toByte
    def asShort: Short   = value.toShort
    def asInt: Int       = value.toInt
    def asLong: Long     = value.toLong
    def asBigInt: BigInt = BigDecimal(value.toDouble).toBigInt
    def asFloat: Float   = value
    def asDouble: Double = value.toDouble
  }

  final case class MDouble(value: Double) extends MNumber {
    def pack(buffer: MessagePacker): Unit =
      buffer.packDouble(value)
    def asByte: Byte     = value.toByte
    def asShort: Short   = value.toShort
    def asInt: Int       = value.toInt
    def asLong: Long     = value.toLong
    def asBigInt: BigInt = BigDecimal(value).toBigInt
    def asFloat: Float   = value.toFloat
    def asDouble: Double = value
  }

  final case class MString(value: String) extends Fmt {
    def isNil: Boolean                                        = false
    def iterator: Iterator[Char]                              = value.iterator
    def getBytes(charset: Charset = UTF_8): Array[scala.Byte] = value.getBytes(charset)
    def getChars: scala.Array[Char]                           = value.toCharArray

    def pack(buffer: MessagePacker): Unit =
      buffer.packString(value)
  }

  final case class MBin(value: Array[Byte]) extends Fmt {
    def isNil: Boolean = false

    def pack(buffer: MessagePacker): Unit = {
      val a = value
      buffer.packBinaryHeader(a.length)
      buffer.writePayload(a)
    }
  }

  final case class MTimestamp(timestamp: Instant) extends Fmt {
    def isNil: Boolean = false

    def pack(buffer: MessagePacker): Unit =
      buffer.packTimestamp(timestamp)
  }

  final case class MExtension(typ: Byte, size: Int, value: Array[Byte]) extends Fmt {
    def isNil: Boolean = false

    def pack(buffer: MessagePacker): Unit = {
      buffer.packExtensionTypeHeader(typ, size)
      buffer.writePayload(value)
    }
  }

  sealed trait Container[A] extends Fmt {
    def isNil: Boolean = false
    def value: A
  }

  sealed trait MMap extends Container[immutable.Map[Fmt, Fmt]] {
    def isEmpty: Boolean
    def apply(key: Fmt): Fmt
    def get(key: Fmt): Option[Fmt]
    def +(entry: (Fmt, Fmt)): MMap
    def ++(other: MMap): MMap
    def map(f: ((Fmt, Fmt)) => (Fmt, Fmt)): MMap
    def flatMap(f: ((Fmt, Fmt)) => MMap): MMap
    final def filter(f: ((Fmt, Fmt)) => Boolean): MMap = MMap(value.filter(f))
    def iterator: Iterator[(Fmt, Fmt)]
  }

  object MMap {
    val empty: MMap = MMap(Map.empty)

    def apply(value: immutable.Map[Fmt, Fmt]): MMap =
      Impl(value)

    def from(entries: (Fmt, Fmt)*): MMap =
      fromSeq(entries)

    def fromSeq(entries: Seq[(Fmt, Fmt)]): MMap =
      Impl(entries.toMap)

    private[mess] def newBuilder: MMap =
      BuilderImpl(Map.newBuilder)

    final private case class Impl(value: immutable.Map[Fmt, Fmt]) extends MMap {
      def apply(key: Fmt): Fmt       = value(key)
      def isEmpty: Boolean           = value.isEmpty
      def get(key: Fmt): Option[Fmt] = value.get(key)
      def +(entry: (Fmt, Fmt)): MMap = Impl(value + entry)

      def ++(other: MMap): MMap =
        if (other.isEmpty) this else Impl(other.value ++ value)

      def map(f: ((Fmt, Fmt)) => (Fmt, Fmt)): MMap =
        if (value.isEmpty) this else Impl(value.map(f))

      def foreach(f: ((Fmt, Fmt)) => Unit): Unit =
        value.foreach(f)

      def flatMap(f: ((Fmt, Fmt)) => MMap): MMap = {
        val it = value.iterator
        @tailrec def go(acc: MMap): MMap =
          if (!it.hasNext) acc else go(acc ++ f(it.next()))
        if (value.isEmpty) this else go(MMap(immutable.Map.empty))
      }
      def iterator: Iterator[(Fmt, Fmt)] = value.iterator

      def pack(acc: MessagePacker): Unit = {
        val a = value
        acc.packMapHeader(a.size)
        a.foreach { case (k, v) =>
          k.pack(acc)
          v.pack(acc)
        }
      }
    }

    final private case class BuilderImpl(private val underlying: mutable.Builder[(Fmt, Fmt), Map[Fmt, Fmt]])
        extends MMap {
      override lazy val value: Map[Fmt, Fmt] = underlying.result()

      def apply(key: Fmt): Fmt       = value(key)
      def isEmpty: Boolean           = value.isEmpty
      def get(key: Fmt): Option[Fmt] = value.get(key)

      def +(entry: (Fmt, Fmt)): MMap = {
        underlying += entry
        this
      }

      def ++(other: MMap): MMap =
        if (other.isEmpty) this
        else {
          underlying ++= other.value
          this
        }

      def map(f: ((Fmt, Fmt)) => (Fmt, Fmt)): MMap =
        if (value.isEmpty) this else Impl(value.map(f))

      def foreach(f: ((Fmt, Fmt)) => Unit): Unit =
        value.foreach(f)

      def flatMap(f: ((Fmt, Fmt)) => MMap): MMap = {
        val it = value.iterator
        @tailrec def go(acc: MMap): MMap =
          if (!it.hasNext) acc else go(acc ++ f(it.next()))
        if (value.isEmpty) this else go(MMap(immutable.Map.empty))
      }
      def iterator: Iterator[(Fmt, Fmt)] = value.iterator

      def pack(acc: MessagePacker): Unit = {
        val a = value
        acc.packMapHeader(a.size)
        a.foreach { case (k, v) =>
          k.pack(acc)
          v.pack(acc)
        }
      }
    }
  }

  sealed trait MArray extends Container[Vector[Fmt]] {
    def isEmpty: Boolean
    def apply(key: scala.Int): Fmt
    def get(key: scala.Int): Option[Fmt]
    def +(entry: Fmt): MArray
    def ++(other: MArray): MArray
    def map(f: Fmt => Fmt): MArray
    def flatMap(f: Fmt => MArray): MArray
    final def filter(f: Fmt => Boolean): MArray = MArray(value.filter(f))
    def iterator: Iterator[Fmt]
    def size: Int
  }

  object MArray {
    val empty: MArray = Impl(Vector.empty)

    def apply(value: Vector[Fmt]): MArray         = Impl(value)
    def unapplySeq(arg: MArray): Option[Seq[Fmt]] = Some(arg.value)
    def from(values: Fmt*): MArray                = Impl(values.toVector)

    final private case class Impl(value: Vector[Fmt]) extends MArray {
      def apply(key: Int): Fmt = value(key)
      def isEmpty: Boolean     = value.isEmpty

      def get(key: Int): Option[Fmt] =
        if (value.size >= key || 0 > key) None else Some(value(key))

      def +(entry: Fmt): MArray =
        MArray(value :+ entry)

      def ++(other: MArray): MArray =
        if (other.isEmpty) this else MArray(value ++ other.value)

      def map(f: Fmt => Fmt): MArray =
        if (value.isEmpty) this else MArray(value.map(f))

      def foreach(f: Fmt => Unit): Unit =
        value.foreach(f)

      def flatMap(f: Fmt => MArray): MArray = {
        val it = value.iterator
        @tailrec def go(acc: MArray): MArray =
          if (!it.hasNext) acc else go(acc ++ f(it.next()))
        if (value.isEmpty) this else go(MArray(Vector.empty))
      }

      def iterator: Iterator[Fmt] =
        value.iterator

      def :+(elem: Fmt): MArray =
        MArray(value.:+(elem))

      def +:(elem: Fmt): MArray =
        MArray(value.+:(elem))

      def pack(acc: MessagePacker): Unit = {
        val a = value
        acc.packArrayHeader(a.size)
        a.foreach(_.pack(acc))
      }
      def size: Int = value.size
    }
  }

  private def unpackArr(buffer: MessageUnpacker)(implicit factory: Factory[Fmt, Vector[Fmt]]): Vector[Fmt] = {
    @tailrec def loop(size: Int, acc: mutable.Builder[Fmt, Vector[Fmt]]): Vector[Fmt] =
      if (size == 0) acc.result()
      else loop(size - 1, acc += unpack(buffer))
    loop(buffer.unpackArrayHeader(), factory.newBuilder)
  }

  private def unpackMap(
    buffer: MessageUnpacker
  )(implicit factory: Factory[(Fmt, Fmt), Map[Fmt, Fmt]]): Map[Fmt, Fmt] = {
    @tailrec def loop(size: Int, acc: mutable.Builder[(Fmt, Fmt), Map[Fmt, Fmt]]): Map[Fmt, Fmt] =
      if (size == 0) acc.result()
      else {
        val k = unpack(buffer)
        val v = unpack(buffer)
        loop(size - 1, acc += k -> v)
      }
    loop(buffer.unpackMapHeader(), factory.newBuilder)
  }

  def pack(msgPack: Fmt, config: MessagePack.PackerConfig = MessagePack.DEFAULT_PACKER_CONFIG): Array[Byte] = {
    val buffer = config.newBufferPacker()
    try {
      msgPack.pack(buffer)
      buffer.toByteArray
    } finally buffer.close()
  }

  def unpack(bytes: Array[Byte], config: MessagePack.UnpackerConfig = MessagePack.DEFAULT_UNPACKER_CONFIG): Fmt = {
    val buffer = config.newUnpacker(bytes)
    try unpack(buffer)
    finally buffer.close()
  }

  private val bigintCriteria = BigInteger.valueOf(9223372036854775807L)

  def unpack(buffer: MessageUnpacker): Fmt =
    if (!buffer.hasNext) Fmt.MUnit
    else
      buffer.getNextFormat match {
        case MF.NIL =>
          buffer.unpackNil()
          Fmt.MNil
        case MF.BOOLEAN =>
          Fmt.MBool(buffer.unpackBoolean())
        case MF.FLOAT32 =>
          Fmt.MFloat(buffer.unpackFloat())
        case MF.FLOAT64 =>
          Fmt.MDouble(buffer.unpackDouble())
        case MF.POSFIXINT | MF.NEGFIXINT | MF.INT8 =>
          Fmt.MByte(buffer.unpackByte())
        case MF.UINT8 =>
          val x = buffer.unpackShort()
          if (x < 128) Fmt.MByte(x.toByte) else Fmt.MShort(x)
        case MF.INT16 =>
          Fmt.MShort(buffer.unpackShort())
        case MF.INT32 =>
          Fmt.MInt(buffer.unpackInt())
        case MF.UINT16 =>
          val x = buffer.unpackInt()
          if (x < 32768) Fmt.MShort(x.toShort) else Fmt.MInt(x)
        case MF.INT64 =>
          Fmt.MLong(buffer.unpackLong())
        case MF.UINT32 =>
          val x = buffer.unpackLong()
          if (x < 2147483648L) Fmt.MInt(x.toInt) else Fmt.MLong(x)
        case MF.UINT64 =>
          val x = buffer.unpackBigInteger()
          if (x.compareTo(bigintCriteria) <= 0)
            Fmt.MLong(x.longValue())
          else
            Fmt.MBigInt(BigInt(x))
        case MF.STR8 | MF.STR16 | MF.STR32 | MF.FIXSTR =>
          Fmt.MString(buffer.unpackString())
        case MF.FIXARRAY | MF.ARRAY16 | MF.ARRAY32 =>
          Fmt.MArray(unpackArr(buffer))
        case MF.FIXMAP | MF.MAP16 | MF.MAP32 =>
          Fmt.MMap(unpackMap(buffer))
        case MF.BIN8 | MF.BIN16 | MF.BIN32 =>
          val size = buffer.unpackBinaryHeader()
          Fmt.MBin(buffer.readPayload(size))
        case MF.EXT8 | MF.EXT16 | MF.EXT32 | MF.FIXEXT1 | MF.FIXEXT2 | MF.FIXEXT4 | MF.FIXEXT8 | MF.FIXEXT16 =>
          val ext = buffer.unpackExtensionTypeHeader()
          if (ext.isTimestampType()) MTimestamp(buffer.unpackTimestamp())
          else {
            val bytes = Array.ofDim[Byte](ext.getLength)
            buffer.readPayload(bytes)
            Fmt.MExtension(ext.getType, ext.getLength, bytes)
          }
        case MF.NEVER_USED =>
          sys.error("cannot unpack format: NEVER USED")
      }

  def nil: Fmt                                                 = MNil
  def unit: Fmt                                                = MUnit
  def fromBoolean(value: Boolean): Fmt                         = MBool(value)
  def fromByte(value: Byte): Fmt                               = MByte(value)
  def fromShort(value: Short): Fmt                             = MShort(value)
  def fromInt(value: Int): Fmt                                 = MInt(value)
  def fromLong(value: Long): Fmt                               = MLong(value)
  def fromBigInt(value: BigInt): Fmt                           = MBigInt(value)
  def fromFloat(value: Float): Fmt                             = MFloat(value)
  def fromDouble(value: Double): Fmt                           = MDouble(value)
  def fromString(value: String): Fmt                           = MString(value)
  def fromMap(value: immutable.Map[Fmt, Fmt]): Fmt             = MMap(value)
  def fromEntries(value: (Fmt, Fmt)*): Fmt                     = MMap.fromSeq(value)
  def fromVector(value: Vector[Fmt]): Fmt                      = MArray(value)
  def fromLsit(value: List[Fmt]): Fmt                          = MArray(value.toVector)
  def fromSeq(value: Seq[Fmt]): Fmt                            = MArray(value.toVector)
  def fromValues(value: Fmt*): Fmt                             = MArray(value.toVector)
  def fromBytes(value: Array[Byte]): Fmt                       = MBin(value)
  def fromInstant(value: Instant): Fmt                         = MTimestamp(value)
  def extension(typ: Byte, size: Int, bytes: Array[Byte]): Fmt = MExtension(typ, size, bytes)
}
