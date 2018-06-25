package mess.codec

import shapeless.Lazy

package object generic {

  def derivedEncoder[A](implicit A: Lazy[DerivedEncoder[A]]): DerivedEncoder[A] = A.value
  def derivedDecoder[A](implicit A: Lazy[DerivedDecoder[A]]): DerivedDecoder[A] = A.value
}
