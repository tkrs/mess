package mess

import mess.codec.Decoder
import mess.codec.Encoder
import munit.FunSuite
import org.msgpack.core.MessageBufferPacker
import org.msgpack.core.MessagePack
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Shrink

trait MsgpackHelper extends FunSuite {

  var packer: MessageBufferPacker = _

  override def beforeEach(context: BeforeEach): Unit =
    packer = MessagePack.DEFAULT_PACKER_CONFIG.newBufferPacker()

  override def afterEach(context: AfterEach): Unit =
    packer.close()

  def roundTrip[A: Arbitrary: Shrink](implicit encode: Encoder[A], decode: Decoder[A]) =
    Prop.forAll { (a: A) =>
      val ast = encode(a)
      ast.pack(packer)
      val bytes    = packer.toByteArray
      val unpacker = MessagePack.DEFAULT_UNPACKER_CONFIG.newUnpacker(bytes)
      val actual   = decode(Fmt.unpack(unpacker)).toTry.get
      packer.clear()
      actual == a
    }

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
