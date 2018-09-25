package mess.ast

import org.msgpack.core.MessagePacker

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
}
