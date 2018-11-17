package dualnumbers

class Dual(val value: Double, val derivative: Double) {
  import Implicits._

  def this(const: Double) = this(const, 0)

  def isDefined: Boolean = value.isReal && derivative.isReal

  def +(that: Dual): Dual = new Dual(value + that.value, derivative + that.derivative)

  def -(that: Dual): Dual = this + (-that)

  def unary_-(): Dual = new Dual(-value, -derivative)

  def *(that: Dual): Dual = new Dual(value * that.value, value * that.derivative + derivative * that.value)

  def /(that: Dual): Dual =
    new Dual(value / that.value, (derivative * that.value - value * that.derivative) / math.pow(that.value, 2))

  def ^(alpha: Double): Dual = new Dual(math.pow(value, alpha), alpha * math.pow(value, alpha-1) * derivative)

  override def toString: String = s"value: $value, derivative: $derivative"

}

object Dual {
  import language.implicitConversions
  implicit def constantToDual[T](a: T)(implicit num: Numeric[T]): Dual = new Dual(num.toDouble(a))

  def sqrt(d: Dual): Dual = {
    val sq: Double = math.sqrt(d.value)
    new Dual(sq, 0.5 * d.derivative / sq)
  }

  def pow(d: Dual, alpha: Double): Dual = d ^ alpha

  def pow(d: Dual, e: Dual): Dual = exp(e * log(d))

  def sin(d: Dual): Dual = new Dual(math.sin(d.value), math.cos(d.value) * d.derivative)

  def cos(d: Dual): Dual = new Dual(math.cos(d.value), -math.sin(d.value) * d.derivative)

  def tan(d: Dual): Dual = new Dual(math.tan(d.value), d.derivative / math.pow(math.cos(d.value), 2))

  def cot(d: Dual): Dual = 1 / tan(d)

  def log(d: Dual): Dual = new Dual(math.log(d.value), d.derivative / d.value)

  def exp(d: Dual): Dual = {
    val ex: Double = math.exp(d.value)
    new Dual(ex, ex * d.derivative)
  }

  def reLu(d: Dual): Dual = new Dual(math.max(0, d.value), (if (d.value <= 0) 0.0 else 1.0) * d.derivative)

  def sigmoid(d: Dual): Dual = 1 / (1 + exp(-d))

}

object Implicits {
  implicit class RichDouble(a: Double) {
    def isReal: Boolean = !a.isInfinity && !a.isNaN
  }
}
