package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "s1 must contains 1")
      assert(!contains(s1, 2), "s1 must not contains 2")
      assert(!contains(s1, 3), "s1 must not contains 3")

      assert(contains(s2, 2), "s2 must contains 2")
      assert(!contains(s2, 1), "s2 must not contains 1")
      assert(!contains(s2, 3), "s2 must not contains 3")

      assert(contains(s3, 3), "s3 must contains 3")
      assert(!contains(s3, 1), "s3 must not contains 1")
      assert(!contains(s3, 2), "s3 must not contains 2")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)

      assert(contains(s, 1), "Union 1 must contains 1")
      assert(contains(s, 2), "Union 1 must contains 2")
      assert(!contains(s, 3), "Union 1 must not contains 3")
    }
  }

  test("intersect contains same elements") {
    new TestSets {
      val i1 = intersect(s1, s2)

      assert(!contains(i1, 1), "Intersect 1 must not contains 1")
      assert(!contains(i1, 2), "Intersect 1 must not contains 2")
      assert(!contains(i1, 3), "Intersect 1 must not contains 3")

      val i2 = intersect(s1, s1)

      assert(contains(i2, 1), "Intersect 2 must contains 1")
      assert(!contains(i2, 2), "Intersect 2 must not contains 2")
      assert(!contains(i2, 3), "Intersect 2 must not contains 3")
    }
  }

  test("diff does not contains same elements") {
    new TestSets {
      val d1 = diff(s1, s2)

      assert(contains(d1, 1), "Diff 1 must contains 1")
      assert(!contains(d1, 2), "Diff 1 must not contains 2")
      assert(!contains(d1, 3), "Diff 1 must not contains 3")

      val d2 = diff(s1, s1)

      assert(!contains(d2, 1), "Diff 2 must not contains 1")
      assert(!contains(d2, 2), "Diff 2 must not contains 2")
      assert(!contains(d2, 3), "Diff 2 must not contains 3")
    }
  }

  test("filter should filter set elements") {
    new TestSets {
      val set = union(union(s1, s2), s3)
      val filteredSet = filter(set, x => x % 2 == 0)

      assert(!contains(filteredSet, 1), "Filtered set must not contains 1")
      assert(contains(filteredSet, 2), "Filtered set must contains 2")
      assert(!contains(filteredSet, 3), "Filtered set must not contains 3")
    }
  }

  test("exists should check if an element exist in given set") {
    new TestSets {
      val set = union(s1, s2)

      assert(exists(set, x => x == 1), "1 should exist in given set")
      assert(exists(set, x => x == 2), "2 should exist in given set")
      assert(!exists(set, x => x == 3), "3 should not exist in given set")
    }
  }

  test("map should check if a mapped element exist in given set") {
    new TestSets {
      val set = union(s1, s2)
      val mapSetted = map(set, x => x + 1)

      assert(contains(mapSetted, 2), "2 should exist in mapped set")
      assert(contains(mapSetted, 3), "3 should exist in mapped set")
      assert(!contains(mapSetted, 1), "1 should not exist in mapped set")
    }
  }
}
