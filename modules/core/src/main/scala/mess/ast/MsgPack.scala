package mess.ast

import org.msgpack.core.{MessagePacker, MessageUnpacker, MessageFormat => MF}

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait MsgPack {
  def pack(acc: MessagePacker): Unit
}
object MsgPack {
  final case object MEmpty extends MsgPack {
    def pack(acc: MessagePacker): Unit = ()
  }
  final case object MNil extends MsgPack {
    def pack(acc: MessagePacker): Unit = acc.packNil()
  }
  final case class MBool(a: Boolean) extends MsgPack {
    def pack(acc: MessagePacker): Unit = acc.packBoolean(a)
  }
  final case class MString(a: String) extends MsgPack {
    def pack(acc: MessagePacker): Unit = acc.packString(a)
  }
  final case class MByte(a: Byte) extends MsgPack {
    def pack(acc: MessagePacker): Unit = acc.packByte(a)
  }
  final case class MShort(a: Short) extends MsgPack {
    def pack(acc: MessagePacker): Unit = acc.packShort(a)
  }
  final case class MInt(a: Int) extends MsgPack {
    def pack(acc: MessagePacker): Unit = acc.packInt(a)
  }
  final case class MLong(a: Long) extends MsgPack {
    def pack(acc: MessagePacker): Unit = acc.packLong(a)
  }
  final case class MBigInt(a: BigInt) extends MsgPack {
    def pack(acc: MessagePacker): Unit = acc.packBigInteger(a.bigInteger)
  }
  final case class MDouble(a: Double) extends MsgPack {
    def pack(acc: MessagePacker): Unit = acc.packDouble(a)
  }
  final case class MFloat(a: Float) extends MsgPack {
    def pack(acc: MessagePacker): Unit = acc.packFloat(a)
  }
  final case class MArray(a: Vector[MsgPack]) extends MsgPack {
    def pack(acc: MessagePacker): Unit = {
      acc.packArrayHeader(a.size)
      val it = a.iterator
      while (it.hasNext) {
        it.next.pack(acc)
      }
    }
  }
  final case class MMap(a: mutable.LinkedHashMap[MsgPack, MsgPack]) extends MsgPack {
    def pack(acc: MessagePacker): Unit = {
      acc.packMapHeader(a.size)
      val it = a.iterator
      while (it.hasNext) {
        val (k, v) = it.next
        k.pack(acc)
        v.pack(acc)
      }
    }

    def add(k: MsgPack, v: MsgPack): MsgPack = {
      this.copy(a += k -> v)
    }
  }
  final case class MExtension(typ: Byte, size: Int, a: Array[Byte]) extends MsgPack {
    def pack(acc: MessagePacker): Unit = {
      acc.packExtensionTypeHeader(typ, size)
      acc.writePayload(a)
    }
  }

  @tailrec private[this] def unpackArr(size: Int,
                                       acc: mutable.Builder[MsgPack, Vector[MsgPack]],
                                       buffer: MessageUnpacker): Vector[MsgPack] = {
    if (size == 0) acc.result()
    else {
      val v = unpack(buffer)
      unpackArr(size - 1, acc += v, buffer)
    }
  }

  @tailrec private[this] def unpackMap(size: Int,
                                       acc: mutable.LinkedHashMap[MsgPack, MsgPack],
                                       buffer: MessageUnpacker): mutable.LinkedHashMap[MsgPack, MsgPack] = {
    if (size == 0) acc
    else {
      val k = unpack(buffer)
      val v = unpack(buffer)
      unpackMap(size - 1, acc += k -> v, buffer)
    }
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
        case MF.POSFIXINT | MF.NEGFIXINT | MF.INT8 | MF.INT16 | MF.INT32 | MF.UINT8 | MF.UINT16 =>
          MsgPack.MInt(buffer.unpackInt())
        case MF.UINT32 | MF.INT64 =>
          MsgPack.MLong(buffer.unpackLong())
        case MF.UINT64 =>
          MsgPack.MBigInt(BigInt(buffer.unpackBigInteger()))
        case MF.BIN8 | MF.BIN16 | MF.BIN32 | MF.STR8 | MF.STR16 | MF.STR32 | MF.FIXSTR =>
          MsgPack.MString(buffer.unpackString())
        case MF.FIXARRAY | MF.ARRAY16 | MF.ARRAY32 =>
          val size = buffer.unpackArrayHeader()
          MsgPack.MArray(unpackArr(size, Vector.newBuilder, buffer))
        case MF.FIXMAP | MF.MAP16 | MF.MAP32 =>
          val size = buffer.unpackMapHeader()
          MsgPack.MMap(unpackMap(size, mutable.LinkedHashMap.empty, buffer))
        case MF.EXT8 | MF.EXT16 | MF.EXT32 | MF.FIXEXT1 | MF.FIXEXT2 | MF.FIXEXT4 | MF.FIXEXT8 | MF.FIXEXT16 =>
          val ext   = buffer.unpackExtensionTypeHeader()
          val bytes = Array.ofDim[Byte](ext.getLength)
          buffer.readPayload(bytes)
          MsgPack.MExtension(ext.getType, ext.getLength, bytes)
        case MF.NEVER_USED =>
          throw new IllegalStateException("NEVER USED")
      }
  }
}
