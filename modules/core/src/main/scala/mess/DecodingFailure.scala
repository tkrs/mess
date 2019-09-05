package mess

import scala.util.control.NoStackTrace

sealed trait DecodingFailure extends NoStackTrace

final case class TypeMismatchError(to: String, from: Fmt) extends DecodingFailure {
  override def getMessage: String = s"cannot decode to $to from ${from.getClass.getSimpleName}."
}
