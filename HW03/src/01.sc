class Complex(val re: Double, val im: Double) {
  override def toString = {
    s"${re.toString} ${if (im >= 0) "+" else "-"} ${im.abs.toString}i"
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Complex]

  override def equals(other: Any): Boolean = other match {
    case that: Complex =>
      (that canEqual this) &&
        re == that.re &&
        im == that.im
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(re, im)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  def conjugation = new Complex(re, im * -1)

  def +(other: Complex) = new Complex(re + other.re, im + other.im)

  def -(other: Complex) = new Complex(re - other.re, im + other.im)

  def *(other: Complex) = new Complex(re * other.re - im * other.im,
    re * other.im + im * other.re)

  private def /(other: Double) = new Complex(re / other, im / other)
  def /(other: Complex): Complex = (this * other.conjugation) / (other * other.conjugation).re

  def unary_+() = this
  def unary_-() = new Complex(-re, -im)

  private val radius = Math.sqrt(re * re + im * im)
  private val angle = Math.atan2(im, re)

  def ^(power: Complex) = {
    val newRadius = Math.pow(radius, power.re) * Math.exp( - power.im * angle)
    val newArgument = power.im * Math.log(radius) + power.re * angle
    new Complex(Math.cos(newArgument) * newRadius, Math.sin(newArgument) * newRadius)
  }

  def Re() = re
  def Im() = im

  def sqrt() = this ^ (new Complex(0.5, 0))
  def abs() = radius
}
object Complex {
  private val doublePattern = "\\d+(?:\\.\\d+)?"
  private val pattern = s"(-?${doublePattern})?\\s*([+-])?\\s*((${doublePattern})?i)?".r
//  private val reOnlyPattern = s"(-?${doublePattern})".r
//  private val imOnlyPattern = s"(-?${doublePattern})i".r
  def apply(re: Double, im: Double) = {
    new Complex(re, im)
  }
  def apply(strRepr: String) = {
    strRepr match {
      case pattern(null, sign, imPart, null) => construct(0, sign, 1)
      case pattern(null, sign, imPart, im) => construct(0, sign, im.toDouble)
      case pattern(re, sign, null, null) => construct(re.toDouble, sign, 0)
      case pattern(re, sign, imPart, null) => construct(re.toDouble, sign, 1)
      case pattern(re, sign, imPart, im) => construct(re.toDouble, sign, im.toDouble)
    }
  }

  def unapply(complex: Complex): Option[(Double, Double)] = Some((complex.re, complex.im))

  private def construct(re: Double, sign: String, im: Double) = {
    sign match {
      case "+" | null => new Complex(re, im)
      case "-" => new Complex(re, im * -1)
    }
  }
}
Complex("-2 - 3i") == Complex(-2, -3.0)
Complex("-2 - 3i") + Complex("2 + 3i") == Complex(0,0)
Complex("1 - i") * Complex("1 - i").conjugation == Complex(2,0)
Complex("4 + 2i") / Complex("3 - i") == Complex("1 + i")
-Complex("-i") == +Complex("i")
Complex("2.0")
Complex("i")
Complex(0,0)
