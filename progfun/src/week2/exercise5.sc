package week2

object exercise5 {
  val x = new Rational2(1, 3)                     //> x  : week2.Rational2 = 1/3
  val y = new Rational2(5, 7)                     //> y  : week2.Rational2 = 5/7
  val z = new Rational2(3, 2)                     //> z  : week2.Rational2 = 3/2

  x.less(y)                                       //> res0: Boolean = true
  x.max(y)                                        //> res1: week2.Rational2 = 5/7

	x.add(y)                                  //> res2: week2.Rational2 = 22/21
	x add y                                   //> res3: week2.Rational2 = 22/21
  new Rational2(2)                                //> res4: week2.Rational2 = 2/1
}

class Rational2(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  val g = gcd(x, y)
  def numer = x / g
  def denom = y / g

  def add(that: Rational2) =
    new Rational2(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def less(that: Rational2) = numer * that.denom < that.numer * denom

  def max(that: Rational2) = if (this.less(that)) that else this

  def neg: Rational2 = new Rational2(-numer, denom)

  def sub(that: Rational2): Rational2 = add(that.neg)

  override def toString = numer + "/" + denom
}