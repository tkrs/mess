package mess.codec

import mess.Fmt

import scala.compiletime.constValue
import scala.compiletime.erasedValue
import scala.compiletime.summonFrom
import scala.deriving.*

private[codec] trait MirrorDecoder:

  inline def summonDecoder[A]: Decoder[A] =
    summonFrom {
      case decodeA: Decoder[A] => decodeA
      case _: Mirror.Of[A]     => derived[A]
    }

  inline def summonLabels[T <: Tuple]: Array[String]       = summonLabelsRec[T].toArray
  inline def summonDecoders[T <: Tuple]: Array[Decoder[?]] = summonDecodersRec[T].toArray

  inline def summonLabelsRec[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].asInstanceOf[String] :: summonLabelsRec[ts]

  inline def summonDecodersRec[T <: Tuple]: List[Decoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonDecoder[t] :: summonDecodersRec[ts]

  def iterator[T](t: T) = t.asInstanceOf[Product].productIterator

  def decodeSum[T](s: Mirror.SumOf[T], names: => Array[String], decoders: => Array[Decoder[?]]): Decoder[T] =
    new Decoder[T]:
      def apply(m: Fmt): Either[DecodingFailure, T] =
        m match
          case m: Fmt.MMap =>
            val it = m.iterator

            val k =
              if (!it.hasNext) None
              else
                val k0 = it.next
                if (it.hasNext)
                  None
                else
                  Some(k0)

            k match
              case Some((k: Fmt.MString, v: Fmt)) =>
                val ord = names.indexOf(k.value)
                if (ord < 0) Left(TypeMismatchError("Sum", m))
                else
                  val decode = decoders(ord).asInstanceOf[Decoder[T]]
                  decode(v)
              case _ =>
                Left(TypeMismatchError("Sum", m))
          case _ =>
            Left(TypeMismatchError("Sum", m))

  def decodeProduct[T](p: Mirror.ProductOf[T], names: => Array[String], decoders: => Array[Decoder[?]]): Decoder[T] =
    new Decoder[T]:
      def apply(m: Fmt): Either[DecodingFailure, T] =
        m match
          case v: Fmt.MMap =>
            val it = v.iterator

            def loop(acc: Array[Any]): Either[DecodingFailure, T] =
              if (!it.hasNext)
                try Right(p.fromProduct(Tuple.fromArray(acc)))
                catch
                  case e: ClassCastException =>
                    throw new Exception(s"${acc.toList}", e)
              else
                val (k, v) = it.next

                k match
                  case k: Fmt.MString =>
                    val ord    = names.indexOf(k.value)
                    val decode = decoders(ord)
                    decode(v) match
                      case Right(v) =>
                        acc(ord) = v
                        loop(acc)
                      case l =>
                        l.asInstanceOf[Either[DecodingFailure, T]]
                  case _ =>
                    Left(TypeMismatchError("Product", m))

            loop(Array.ofDim[Any](names.size))
          case _ =>
            Left(TypeMismatchError("Product", m))

  inline given derived[T](using m: Mirror.Of[T]): Decoder[T] =
    lazy val elemNames     = summonLabels[m.MirroredElemLabels]
    lazy val elemInstances = summonDecoders[m.MirroredElemTypes]

    inline m match
      case s: Mirror.SumOf[T] =>
        decodeSum(s, elemNames, elemInstances)
      case p: Mirror.ProductOf[T] =>
        decodeProduct(p, elemNames, elemInstances)
