package mess.ast

import scala.collection.mutable

sealed trait MsgPack
object MsgPack {
  final case object MEmpty                                          extends MsgPack
  final case object MNil                                            extends MsgPack
  final case class MBool(a: Boolean)                                extends MsgPack
  final case class MString(a: String)                               extends MsgPack
  final case class MByte(a: Byte)                                   extends MsgPack
  final case class MExtension(typ: Byte, size: Int, a: Array[Byte]) extends MsgPack
  final case class MShort(a: Short)                                 extends MsgPack
  final case class MInt(a: Int)                                     extends MsgPack
  final case class MLong(a: Long)                                   extends MsgPack
  final case class MBigInt(a: BigInt)                               extends MsgPack
  final case class MDouble(a: Double)                               extends MsgPack
  final case class MFloat(a: Float)                                 extends MsgPack
  final case class MArray(a: Vector[MsgPack])                       extends MsgPack
  final case class MMap(a: mutable.LinkedHashMap[MsgPack, MsgPack]) extends MsgPack {
    def add(k: MsgPack, v: MsgPack): MsgPack = {
      this.copy(a += k -> v)
    }
  }
}
