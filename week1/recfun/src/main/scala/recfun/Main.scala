package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    testBalance()
    testCountChanges()
  }

  def testPascal() {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row) {
        print(pascal(col, row) + " ")
      }
      println()
    }
  }

  def testBalance() {
    def test(chars: String) = println(chars + " => " + balance(chars.toList))

    test("(if (zero? x) max (/ 1 x))")
    test("I told him (that it’s not (yet) done). (But he wasn’t listening)")
    test(":-)")
    test("())(")
    test("()(")
  }

  def testCountChanges() {
    def test(money: Int, coins: List[Int]) = println(money + " with " + coins + " => " + countChange(money, coins))

    test(4, List(1, 2))
    test(4, List(2, 1))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0 || c > r) 0
    else if (c == 0 || r == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(nb: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) nb == 0
      else {
        val first = chars.head
        val inc =
          if (first == '(') 1
          else if (first == ')') -1
          else 0

        val newNb = nb + inc

        if (newNb < 0) false
        else loop(newNb, chars.tail)
      }
    }

    if (chars.isEmpty) true
    else loop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(n: Int, m: List[Int]): Int = {
      if (n == 0) 1
      else if (n < 0 || m.isEmpty) 0
      else if (n > 1 && m.isEmpty) 0
      else {
        loop(n, m.tail) + loop(n - m.head, m)
      }
    }

    loop(money, coins.sorted)
  }
}
