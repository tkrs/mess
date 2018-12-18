package mess.ast

import java.math.BigInteger

import org.msgpack.core.{MessagePack, MessagePacker, MessageUnpacker, MessageFormat => MF}

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait MsgPack {

  def isNil: Boolean

  def isEmpty: Boolean

  def pack(acc: MessagePacker): Unit

  def asBoolean: Option[Boolean]

  def asByteArray: Option[Array[Byte]]

  def asExtension: Option[(Byte, Int, Array[Byte])]

  def asByte: Option[Byte]

  def asShort: Option[Short]

  def asInt: Option[Int]

  def asLong: Option[Long]

  def asBigInt: Option[BigInt]

  def asDouble: Option[Double]

  def asFloat: Option[Float]

  def asChar: Option[Char]

  def asString: Option[String]

  def asMap: Option[Map[MsgPack, MsgPack]]

  def asVector: Option[Vector[MsgPack]]
}

object MsgPack {

  private[mess] final case object MEmpty extends MsgPack {
    def isNil: Boolean                                = false
    def isEmpty: Boolean                              = true
    def pack(acc: MessagePacker): Unit                = ()
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = None
    def asShort: Option[Short]                        = None
    def asInt: Option[Int]                            = None
    def asLong: Option[Long]                          = None
    def asBigInt: Option[BigInt]                      = None
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case object MNil extends MsgPack {
    def isNil: Boolean                                = true
    def isEmpty: Boolean                              = false
    def pack(acc: MessagePacker): Unit                = acc.packNil()
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = None
    def asShort: Option[Short]                        = None
    def asInt: Option[Int]                            = None
    def asLong: Option[Long]                          = None
    def asBigInt: Option[BigInt]                      = None
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case class MBool(a: Boolean) extends MsgPack {
    def isNil: Boolean                                = false
    def isEmpty: Boolean                              = false
    def pack(acc: MessagePacker): Unit                = acc.packBoolean(a)
    def asBoolean: Option[Boolean]                    = Some(a)
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = None
    def asShort: Option[Short]                        = None
    def asInt: Option[Int]                            = None
    def asLong: Option[Long]                          = None
    def asBigInt: Option[BigInt]                      = None
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case class MString(a: String) extends MsgPack {
    def isNil: Boolean                                = false
    def isEmpty: Boolean                              = false
    def pack(acc: MessagePacker): Unit                = acc.packString(a)
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = None
    def asShort: Option[Short]                        = None
    def asInt: Option[Int]                            = None
    def asLong: Option[Long]                          = None
    def asBigInt: Option[BigInt]                      = None
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = if (a.length == 1) Some(a.charAt(0)) else None
    def asString: Option[String]                      = Some(a)
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case class MByte(a: Byte) extends MsgPack {
    def isNil: Boolean                                = false
    def isEmpty: Boolean                              = false
    def pack(acc: MessagePacker): Unit                = acc.packByte(a)
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = Some(a)
    def asShort: Option[Short]                        = Some(a.toShort)
    def asInt: Option[Int]                            = Some(a.toInt)
    def asLong: Option[Long]                          = Some(a.toLong)
    def asBigInt: Option[BigInt]                      = Some(BigInt(a.toInt))
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case class MShort(a: Short) extends MsgPack {
    def isNil: Boolean                                = false
    def isEmpty: Boolean                              = false
    def pack(acc: MessagePacker): Unit                = acc.packShort(a)
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = Some(a.toByte)
    def asShort: Option[Short]                        = Some(a)
    def asInt: Option[Int]                            = Some(a.toInt)
    def asLong: Option[Long]                          = Some(a.toLong)
    def asBigInt: Option[BigInt]                      = Some(BigInt(a.toInt))
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case class MInt(a: Int) extends MsgPack {
    def isNil: Boolean                                = false
    def isEmpty: Boolean                              = false
    def pack(acc: MessagePacker): Unit                = acc.packInt(a)
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = Some(a.toByte)
    def asShort: Option[Short]                        = Some(a.toShort)
    def asInt: Option[Int]                            = Some(a.toInt)
    def asLong: Option[Long]                          = Some(a.toLong)
    def asBigInt: Option[BigInt]                      = Some(BigInt(a))
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case class MLong(a: Long) extends MsgPack {
    def isNil: Boolean                                = false
    def isEmpty: Boolean                              = false
    def pack(acc: MessagePacker): Unit                = acc.packLong(a)
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = Some(a.toByte)
    def asShort: Option[Short]                        = Some(a.toShort)
    def asInt: Option[Int]                            = Some(a.toInt)
    def asLong: Option[Long]                          = Some(a)
    def asBigInt: Option[BigInt]                      = Some(BigInt(a))
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case class MBigInt(a: BigInt) extends MsgPack {
    def isNil: Boolean                                = false
    def isEmpty: Boolean                              = false
    def pack(acc: MessagePacker): Unit                = acc.packBigInteger(a.bigInteger)
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = Some(a.toByte)
    def asShort: Option[Short]                        = Some(a.toShort)
    def asInt: Option[Int]                            = Some(a.toInt)
    def asLong: Option[Long]                          = Some(a.toLong)
    def asBigInt: Option[BigInt]                      = Some(a)
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case class MDouble(a: Double) extends MsgPack {
    def isNil: Boolean                                = false
    def isEmpty: Boolean                              = false
    def pack(acc: MessagePacker): Unit                = acc.packDouble(a)
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = None
    def asShort: Option[Short]                        = None
    def asInt: Option[Int]                            = None
    def asLong: Option[Long]                          = None
    def asBigInt: Option[BigInt]                      = None
    def asDouble: Option[Double]                      = Some(a)
    def asFloat: Option[Float]                        = Some(a.toFloat)
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case class MFloat(a: Float) extends MsgPack {
    def isNil: Boolean                                = false
    def isEmpty: Boolean                              = false
    def pack(acc: MessagePacker): Unit                = acc.packFloat(a)
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = None
    def asShort: Option[Short]                        = None
    def asInt: Option[Int]                            = None
    def asLong: Option[Long]                          = None
    def asBigInt: Option[BigInt]                      = None
    def asDouble: Option[Double]                      = Some(a.toDouble)
    def asFloat: Option[Float]                        = Some(a)
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case class MArray(a: Vector[MsgPack]) extends MsgPack {
    def isNil: Boolean   = false
    def isEmpty: Boolean = false
    def pack(acc: MessagePacker): Unit = {
      acc.packArrayHeader(a.size)
      a.foreach(_.pack(acc))
    }
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = None
    def asShort: Option[Short]                        = None
    def asInt: Option[Int]                            = None
    def asLong: Option[Long]                          = None
    def asBigInt: Option[BigInt]                      = None
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = Some(a)
  }

  private[mess] final case class MMap(a: Map[MsgPack, MsgPack]) extends MsgPack {
    def isNil: Boolean                                = false
    def isEmpty: Boolean                              = false
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = None
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = None
    def asShort: Option[Short]                        = None
    def asInt: Option[Int]                            = None
    def asLong: Option[Long]                          = None
    def asBigInt: Option[BigInt]                      = None
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = Some(a)
    def asVector: Option[Vector[MsgPack]]             = None
    def pack(acc: MessagePacker): Unit = {
      acc.packMapHeader(a.size)
      a.foreach {
        case (k, v) =>
          k.pack(acc)
          v.pack(acc)
      }
    }
    def iterator: Iterator[(MsgPack, MsgPack)] = a.iterator
  }

  private[mess] final case class MExtension(typ: Byte, size: Int, a: Array[Byte]) extends MsgPack {
    def isNil: Boolean   = false
    def isEmpty: Boolean = false
    def pack(acc: MessagePacker): Unit = {
      acc.packExtensionTypeHeader(typ, size)
      acc.writePayload(a)
    }

    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = Some(a)
    def asExtension: Option[(Byte, Int, Array[Byte])] = Some((typ, size, a))
    def asByte: Option[Byte]                          = None
    def asShort: Option[Short]                        = None
    def asInt: Option[Int]                            = None
    def asLong: Option[Long]                          = None
    def asBigInt: Option[BigInt]                      = None
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  private[mess] final case class MBin(a: Array[Byte]) extends MsgPack {
    def isNil: Boolean   = false
    def isEmpty: Boolean = false
    def pack(acc: MessagePacker): Unit = {
      acc.packBinaryHeader(a.length)
      acc.writePayload(a)
    }
    def asBoolean: Option[Boolean]                    = None
    def asByteArray: Option[Array[Byte]]              = Some(a)
    def asExtension: Option[(Byte, Int, Array[Byte])] = None
    def asByte: Option[Byte]                          = None
    def asShort: Option[Short]                        = None
    def asInt: Option[Int]                            = None
    def asLong: Option[Long]                          = None
    def asBigInt: Option[BigInt]                      = None
    def asDouble: Option[Double]                      = None
    def asFloat: Option[Float]                        = None
    def asChar: Option[Char]                          = None
    def asString: Option[String]                      = None
    def asMap: Option[Map[MsgPack, MsgPack]]          = None
    def asVector: Option[Vector[MsgPack]]             = None
  }

  @tailrec private[this] def unpackArr(size: Int,
                                       acc: mutable.Builder[MsgPack, Vector[MsgPack]],
                                       buffer: MessageUnpacker): Vector[MsgPack] = {
    if (size == 0) acc.result()
    else unpackArr(size - 1, acc += unpack(buffer), buffer)
  }

  @tailrec private[this] def unpackMap(size: Int,
                                       acc: mutable.Builder[(MsgPack, MsgPack), Map[MsgPack, MsgPack]],
                                       buffer: MessageUnpacker): Map[MsgPack, MsgPack] = {
    if (size == 0) acc.result()
    else {
      val k = unpack(buffer)
      val v = unpack(buffer)
      unpackMap(size - 1, acc += k -> v, buffer)
    }
  }

  def pack(msgPack: MsgPack, config: MessagePack.PackerConfig): Array[Byte] = {
    val buffer = config.newBufferPacker()
    try {
      msgPack.pack(buffer)
      buffer.toByteArray
    } finally buffer.close()
  }

  def unpack(bytes: Array[Byte], config: MessagePack.UnpackerConfig): MsgPack = {
    val buffer = config.newUnpacker(bytes)
    try unpack(buffer)
    finally buffer.close()
  }

  def unpack(buffer: MessageUnpacker): MsgPack = {
    if (!buffer.hasNext) MsgPack.MEmpty
    else
      buffer.getNextFormat match {
        case MF.NIL =>
          buffer.unpackNil()
          MsgPack.MNil
        case MF.BOOLEAN =>
          MsgPack.MBool(buffer.unpackBoolean())
        case MF.FLOAT32 =>
          MsgPack.MFloat(buffer.unpackFloat())
        case MF.FLOAT64 =>
          MsgPack.MDouble(buffer.unpackDouble())
        case MF.POSFIXINT | MF.NEGFIXINT | MF.INT8 =>
          MsgPack.MByte(buffer.unpackByte())
        case MF.UINT8 =>
          val x = buffer.unpackShort()
          if (x < 128) MsgPack.MByte(x.toByte) else MsgPack.MShort(x)
        case MF.INT16 =>
          MsgPack.MShort(buffer.unpackShort())
        case MF.INT32 =>
          MsgPack.MInt(buffer.unpackInt())
        case MF.UINT16 =>
          val x = buffer.unpackInt()
          if (x < 32768) MsgPack.MShort(x.toShort) else MsgPack.MInt(x)
        case MF.INT64 =>
          MsgPack.MLong(buffer.unpackLong())
        case MF.UINT32 =>
          val x = buffer.unpackLong()
          if (x < 2147483648L) MsgPack.MInt(x.toInt) else MsgPack.MLong(x)
        case MF.UINT64 =>
          val x = buffer.unpackBigInteger()
          if (x.compareTo(BigInteger.valueOf(9223372036854775807L)) <= 0)
            MsgPack.MLong(x.longValue())
          else
            MsgPack.MBigInt(BigInt(x))
        case MF.STR8 | MF.STR16 | MF.STR32 | MF.FIXSTR =>
          MsgPack.MString(buffer.unpackString())
        case MF.FIXARRAY | MF.ARRAY16 | MF.ARRAY32 =>
          val size    = buffer.unpackArrayHeader()
          val builder = Vector.newBuilder[MsgPack]
          builder.sizeHint(size)
          MsgPack.MArray(unpackArr(size, builder, buffer))
        case MF.FIXMAP | MF.MAP16 | MF.MAP32 =>
          val size    = buffer.unpackMapHeader()
          val builder = Map.newBuilder[MsgPack, MsgPack]
          builder.sizeHint(size)
          MsgPack.MMap(unpackMap(size, builder, buffer))
        case MF.BIN8 | MF.BIN16 | MF.BIN32 =>
          val size = buffer.unpackBinaryHeader()
          MsgPack.MBin(buffer.readPayload(size))
        case MF.EXT8 | MF.EXT16 | MF.EXT32 | MF.FIXEXT1 | MF.FIXEXT2 | MF.FIXEXT4 | MF.FIXEXT8 | MF.FIXEXT16 =>
          val ext   = buffer.unpackExtensionTypeHeader()
          val bytes = Array.ofDim[Byte](ext.getLength)
          buffer.readPayload(bytes)
          MsgPack.MExtension(ext.getType, ext.getLength, bytes)
        case MF.NEVER_USED =>
          throw sys.error("cannot unpack format: NEVER USED")
      }
  }

  private[this] final val True  = MBool(true)
  private[this] final val False = MBool(false)

  def nil: MsgPack                                        = MNil
  def empty: MsgPack                                      = MEmpty
  def fromPairs(xs: (MsgPack, MsgPack)*): MsgPack         = MMap(Map(xs: _*))
  def fromPairSeq(xs: Seq[(MsgPack, MsgPack)]): MsgPack   = MMap(Map(xs: _*))
  def fromMap(xs: Map[MsgPack, MsgPack]): MsgPack         = MMap(xs)
  def fromValues(xs: MsgPack*): MsgPack                   = MArray(xs.toVector)
  def fromVector(xs: Vector[MsgPack]): MsgPack            = MArray(xs)
  def fromBoolean(x: Boolean): MsgPack                    = if (x) True else False
  def fromString(x: String): MsgPack                      = MString(x)
  def fromBigInt(x: BigInt): MsgPack                      = MBigInt(x)
  def fromByte(x: Byte): MsgPack                          = MByte(x)
  def fromShort(x: Short): MsgPack                        = MShort(x)
  def fromInt(x: Int): MsgPack                            = MInt(x)
  def fromLong(x: Long): MsgPack                          = MLong(x)
  def fromFloat(x: Float): MsgPack                        = MFloat(x)
  def fromDouble(x: Double): MsgPack                      = MDouble(x)
  def fromBytes(x: Array[Byte]): MsgPack                  = MBin(x)
  def extension(typ: Byte, size: Int, bytes: Array[Byte]) = MExtension(typ, size, bytes)
}
