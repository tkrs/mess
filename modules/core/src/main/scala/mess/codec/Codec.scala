package mess.codec

import mess.ast.{MsgPack, MutMap}
import org.msgpack.core.{MessagePacker, MessageUnpacker, MessageFormat => MF}

import scala.annotation.tailrec
import scala.collection.mutable

object Codec {

  @tailrec private[this] def deserializeArr(size: Int,
                                            acc: mutable.Builder[MsgPack, Vector[MsgPack]],
                                            buffer: MessageUnpacker): Vector[MsgPack] = {
    if (size == 0) acc.result()
    else {
      val v = deserialize(buffer)
      deserializeArr(size - 1, acc += v, buffer)
    }
  }

  @tailrec private[this] def deserializeMap(size: Int, acc: MutMap, buffer: MessageUnpacker): MutMap = {
    if (size == 0) acc
    else {
      val k = deserialize(buffer)
      val v = deserialize(buffer)
      deserializeMap(size - 1, acc.add(k, v), buffer)
    }
  }

  def deserialize(buffer: MessageUnpacker): MsgPack = {
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
          MsgPack.MArray(deserializeArr(size, Vector.newBuilder, buffer))
        case MF.FIXMAP | MF.MAP16 | MF.MAP32 =>
          val size = buffer.unpackMapHeader()
          MsgPack.MMap(deserializeMap(size, MutMap.empty, buffer))
        case MF.EXT8 | MF.EXT16 | MF.EXT32 | MF.FIXEXT1 | MF.FIXEXT2 | MF.FIXEXT4 | MF.FIXEXT8 | MF.FIXEXT16 =>
          val ext   = buffer.unpackExtensionTypeHeader()
          val bytes = Array.ofDim[Byte](ext.getLength)
          buffer.readPayload(bytes)
          MsgPack.MExtension(ext.getType, ext.getLength, bytes)
        case MF.NEVER_USED =>
          throw new IllegalStateException("NEVER USED")
      }
  }

  def serialize(m: MsgPack, acc: MessagePacker): Unit = m match {
    case MsgPack.MEmpty     => ()
    case MsgPack.MNil       => acc.packNil()
    case MsgPack.MBool(a)   => acc.packBoolean(a)
    case MsgPack.MByte(a)   => acc.packByte(a)
    case MsgPack.MShort(a)  => acc.packShort(a)
    case MsgPack.MInt(a)    => acc.packInt(a)
    case MsgPack.MLong(a)   => acc.packLong(a)
    case MsgPack.MBigInt(a) => acc.packBigInteger(a.bigInteger)
    case MsgPack.MFloat(a)  => acc.packFloat(a)
    case MsgPack.MDouble(a) => acc.packDouble(a)
    case MsgPack.MString(a) => acc.packString(a)
    case MsgPack.MVal(_, a) => serialize(a, acc)
    case MsgPack.MExtension(t, s, a) =>
      acc.packExtensionTypeHeader(t, s)
      acc.writePayload(a)
    case MsgPack.MArray(a) =>
      acc.packArrayHeader(a.size)
      a.foreach(serialize(_, acc))
    case MsgPack.MMap(a) =>
      acc.packMapHeader(a.size)
      a.iterator.foreach {
        case (k, v) =>
          serialize(k, acc)
          serialize(v, acc)
      }
  }
}
