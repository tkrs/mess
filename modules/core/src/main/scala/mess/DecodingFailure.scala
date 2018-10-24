package mess

import mess.ast.MsgPack

import scala.util.control.NoStackTrace

sealed trait DecodingFailure extends NoStackTrace

final case class TypeMismatchError(to: String, from: MsgPack) extends DecodingFailure {
  override def getMessage: String = s"cannot decode to $to from ${from.getClass.getSimpleName}."
}
