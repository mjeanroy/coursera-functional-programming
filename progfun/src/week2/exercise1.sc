package week2

object exercise1 {
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) + acc)

    loop(a, 0)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int

  sum(x => x, 3, 5)                               //> res0: Int = 12
}