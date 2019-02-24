package mess

trait KeyDecoder[S <: Symbol] {
  def apply(a: S): String
}

object KeyDecoder {

  final def instance[A <: Symbol](f: A => String): KeyDecoder[A] = new KeyDecoder[A] {
    def apply(a: A): String = f(a)
  }

  implicit def decodeSymbolKey[S <: Symbol]: KeyDecoder[S] = new KeyDecoder[S] {
    def apply(m: S): String = m.name
  }
}
