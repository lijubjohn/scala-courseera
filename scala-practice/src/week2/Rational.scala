package week2

/**
  * Created by liju on 1/20/17.
  */
class Rational(n: Int, d: Int) {
  require(d != 0)

  private val g = gcd(n, d)

  val numeretor = n / g
  val denominator = d / g

  def this(n: Int) = this(n, 1)

  def add(that: Rational): Rational = new Rational(numeretor * that.denominator + that.numeretor * denominator, denominator * that.denominator)


  override def toString: String = numeretor + "/" + denominator

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }
}
