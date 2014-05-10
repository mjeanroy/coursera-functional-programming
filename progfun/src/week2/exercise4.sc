package week2

object exercise4 {
  val x = new Rational1(1, 3)                     //> x  : week2.Rational1 = 1/3
  val y = new Rational1(5, 7)                     //> y  : week2.Rational1 = 5/7
  val z = new Rational1(3, 2)                     //> z  : week2.Rational1 = 3/2
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 3
  x.add(y)                                        //> res2: week2.Rational1 = 22/21

  x.sub(y).sub(z)                                 //> res3: week2.Rational1 = -79/42
}

class Rational1(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational1) =
    new Rational1(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg: Rational1 = new Rational1(-numer, denom)

  def sub(that: Rational1): Rational1 = add(that.neg)

  override def toString = numer + "/" + denom
}