import sbt._

/**
  * Generate a range of boilerplate classes that would be tedious to write and maintain by hand.
  *
  * Copied, with some modifications, from
  * [[https://github.com/milessabin/shapeless/blob/master/project/Boilerplate.scala Shapeless]].
  *
  * @author Miles Sabin
  * @author Kevin Wright
  */
object Boilerplate {
  import scala.StringContext._

  implicit class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines     = interpolated.split('\n')
      val trimmedLines = rawLines.map(_.dropWhile(_.isWhitespace))
      trimmedLines.mkString("\n")
    }
  }

  val templates: Seq[Template] = List(
    GenTupleEncoder,
    GenTupleDecoder
  )

  /** Returns a seq of the generated files.  As a side-effect, it actually generates them... */
  def gen(dir: File) =
    for (t <- templates) yield {
      val tgtFile = t.file(dir)
      IO.write(tgtFile, t.body)
      tgtFile
    }

  val header   = ""
  val maxArity = 22

  class TemplateVals(val arity: Int) {
    val synTypes     = (0 until arity).map(n => (n + 'A').toChar)
    val synVals      = (0 until arity).map(n => (n + 'a').toChar)
    val synTypedVals = (synVals.zip(synTypes)).map { case (v, t) => v + ":" + t }

    val `A..N`                      = synTypes.mkString(", ")
    val `a..n`                      = synVals.mkString(", ")
    val `(A..N)`                    = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(a..n)`                    = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    def `a:F[A]..n:F[N]`(f: String) = (synVals.zip(synTypes)).map { case (v, t) => s"$v: $f[$t]" }.mkString(", ")
  }

  trait Template {
    def file(root: File): File
    def content(tv: TemplateVals): String
    def range = 1 to maxArity

    def body: String = {
      val headerLines = header.split('\n')
      val rawContents = range.map(n => content(new TemplateVals(n)).split('\n').filterNot(_.isEmpty))
      val preBody     = rawContents.head.takeWhile(_.startsWith("|")).map(_.tail)
      val instances   = rawContents.flatMap(_.filter(_.startsWith("-")).map(_.tail))
      val postBody    = rawContents.head.dropWhile(_.startsWith("|")).dropWhile(_.startsWith("-")).map(_.tail)
      (headerLines ++ preBody ++ instances ++ postBody).mkString("\n")
    }
  }

  /*
    Blocks in the templates below use a custom interpolator, combined with post-processing to produce the body

      - The contents of the `header` val is output first

      - Then the first block of lines beginning with '|'

      - Then the block of lines beginning with '-' is replicated once for each arity,
        with the `templateVals` already pre-populated with relevant relevant vals for that arity

      - Then the last block of lines prefixed with '|'

    The block otherwise behaves as a standard interpolated string with regards to variable substitution.
   */

  object GenTupleEncoder extends Template {
    def file(root: File) = root / "mess" / "codec" / "TupleEncoder.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val arg  = "xs"
      val expr = synVals.zipWithIndex.map { case (f, i) => s"$f($arg._${i + 1})" }.mkString(",")

      block"""
      |package mess.codec
      |
      |import _root_.mess.Fmt
      |
      |private[codec] trait TupleEncoder {
        -  implicit def encodeTuple$arity[${`A..N`}](implicit ${`a:F[A]..n:F[N]`("Encoder")}): Encoder.AsArray[${`(A..N)`}] =
        -    $arg => Fmt.MArray(Vector($expr))
      |}
      """
    }
  }

  object GenTupleDecoder extends Template {
    def file(root: File) = root / "mess" / "codec" / "TupleDecoder.scala"

    def content(tv: TemplateVals) = {
      import tv._

      def g(f: Char, i: Int, b: String): String =
        s"""{$f(xs($i))match{case Right(${f}0)=>$b;case Left(e)=>Left(e)}}"""

      val result = if (synVals.size == 1) s"Tuple1(a0)" else synVals.map(v => s"${v}0").mkString("(", ",", ")")
      val expr   = synVals.zipWithIndex.foldRight(s"Right($result)") { case ((l, i), r) => g(l, i, r) }

      block"""
      |package mess.codec
      |
      |import _root_.mess.Fmt
      |
      |private[codec] trait TupleDecoder {
        -  implicit def decodeTuple$arity[${`A..N`}](implicit ${`a:F[A]..n:F[N]`("Decoder")}): Decoder[${`(A..N)`}] = {
        -    case xs: Fmt.MArray if xs.size == $arity => $expr
        -    case m => Left(TypeMismatchError("(${`A..N`})", m))
        -  }
      |}
      """
    }
  }
}
