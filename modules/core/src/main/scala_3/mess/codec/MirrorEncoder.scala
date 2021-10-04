package mess.codec

import mess.Fmt

import scala.compiletime.constValue
import scala.compiletime.erasedValue
import scala.compiletime.summonFrom
import scala.compiletime.summonInline
import scala.deriving._

private[codec] trait MirrorEncoder:

  inline def summonEncoder[A]: Encoder[A] =
    summonFrom {
      case encodeA: Encoder[A] => encodeA
      case _: Mirror.Of[A]     => derived[A]
    }

  inline def summonLabels[T <: Tuple]: Array[String]       = summonLabelsRec[T].toArray
  inline def summonEncoders[T <: Tuple]: Array[Encoder[_]] = summonEncodersRec[T].toArray

  inline def summonLabelsRec[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].asInstanceOf[String] :: summonLabelsRec[ts]

  inline def summonEncodersRec[T <: Tuple]: List[Encoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonEncoder[t] :: summonEncodersRec[ts]

  def iterator[T](t: T) = t.asInstanceOf[Product].productIterator

  def encoderSum[T](s: Mirror.SumOf[T], elems: => Array[Encoder[_]]): Encoder.AsMap[T] =
    new Encoder.AsMap[T]:
      def applyToMap(v: T): Fmt.MMap =
        val ord    = s.ordinal(v)
        val encode = elems(ord).asInstanceOf[Encoder[T]]
        val value  = encode(v)
        Fmt.MMap.from(Fmt.fromString(ord.toString) -> value)

  def encoderProduct[T](p: Mirror.ProductOf[T],
                        names: => Array[String],
                        encoders: => Array[Encoder[_]]
  ): Encoder.AsMap[T] =
    new Encoder.AsMap[T]:
      def applyToMap(v: T): Fmt.MMap =
        val obj = names
          .zip(iterator(v).toList)
          .zip(encoders)
          .map { case ((k, v), f) => Fmt.fromString(k) -> f.asInstanceOf[Encoder[Any]].apply(v) }
        Fmt.MMap(obj.toMap)

  inline given derived[T](using m: Mirror.Of[T]): Encoder.AsMap[T] =
    lazy val elemNames     = summonLabels[m.MirroredElemLabels]
    lazy val elemInstances = summonEncoders[m.MirroredElemTypes]

    inline m match
      case s: Mirror.SumOf[T] =>
        encoderSum(s, elemInstances)
      case p: Mirror.ProductOf[T] =>
        encoderProduct(p, elemNames, elemInstances)
