package mess

import munit.FunSuite
import org.msgpack.core.MessageBufferPacker
import org.msgpack.core.MessagePack

trait MsgpackHelper extends FunSuite {

  var packer: MessageBufferPacker = _

  override def beforeEach(context: BeforeEach): Unit =
    packer = MessagePack.DEFAULT_PACKER_CONFIG.newBufferPacker()

  override def afterEach(context: AfterEach): Unit =
    packer.close()
}

object MsgpackHelper {
  implicit class BinHelper(val sc: StringContext) extends AnyVal {
    def x(): Array[Byte] = {
      val strings = sc.parts.iterator

      def toByte(s: String): Byte = BigInt(s, 16).toByte

      if (strings.isEmpty) Array.emptyByteArray
      else strings.next().split(" ").map(toByte)
    }
  }
}
