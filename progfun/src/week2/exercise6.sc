package week2

object exercise6 {
  val x = new Rational3(1, 3)                     //> x  : week2.Rational3 = 1/3
  val y = new Rational3(5, 7)                     //> y  : week2.Rational3 = 5/7
  val z = new Rational3(3, 2)                     //> z  : week2.Rational3 = 3/2

  x + y                                           //> res0: week2.Rational3 = 22/21
  x - y                                           //> res1: week2.Rational3 = 8/-21
  x < y                                           //> res2: Boolean = true
  -x                                              //> res3: week2.Rational3 = 1/-3
  -y                                              //> res4: week2.Rational3 = 5/-7
}

class Rational3(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  val g = gcd(x, y)
  def numer = x / g
  def denom = y / g

  def +(that: Rational3) =
    new Rational3(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def < (that: Rational3) = numer * that.denom < that.numer * denom

  def max(that: Rational3) = if (this < that) that else this

  def unary_- : Rational3 = new Rational3(-numer, denom)

  def - (that: Rational3): Rational3 = this + -that

  override def toString = numer + "/" + denom
}