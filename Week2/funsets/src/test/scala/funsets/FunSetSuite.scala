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
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


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
    val s4 = singletonSet(4)
    val s6 = singletonSet(6)

    val s12 = union(s1,s2)
    val s13 = union(s1,s3)
    val s23 = union(s2,s3)
    val s34 = union(s2,s4)
    val s1234 = union(s12, s34)
    val s246 = union(union(s2,s4),s6)
    val s136 = union(union(s1,s3),s6)
    val s123 = union(union(s1,s2),s3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1 but not 2") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only elements in s12 and s23 ie 2") {
    new TestSets {
      val s1223 = intersect(s12, s23)
      assert(!contains(s1223, 1), "Intersect 1")
      assert(contains(s1223, 2), "Intersect 2")
      assert(!contains(s1223, 3), "Intersect 3")
      assert(!contains(s1223, 4), "Intersect 4")
    }
  }

  test("diff contains only elements in s12 not in s23 ie 1") {
    new TestSets {
      val s1223 = diff(s12, s23)
      assert(contains(s1223, 1), "Intersect 1")
      assert(!contains(s1223, 2), "Intersect 2")
      assert(!contains(s1223, 3), "Intersect 3")
      assert(!contains(s1223, 4), "Intersect 4")
    }
  }

  test("filter pair number in s1234 only contains 2 and 4") {
    new TestSets {
      val filtered = filter(s1234, x => x % 2 == 0)
      assert(!contains(filtered, 1), "Intersect 1")
      assert(contains(filtered, 2), "Intersect 2")
      assert(!contains(filtered, 3), "Intersect 3")
      assert(contains(filtered, 4), "Intersect 4")
      assert(!contains(filtered, 5), "Intersect 5")
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(s246, x => x % 2 == 0), "s246 contains only pairs")
      assert(!forall(s23, x => x % 2 == 0), "s23 contains an impair numbers")
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(s136, x => x % 2 == 0), "s136 contains a pairs")
      assert(!exists(s246, x => x % 2 == 1), "s246 does not contains an impair numbers")
    }
  }

  test("map") {
    new TestSets {
      val mapped = map(s123, x => x * 2)
      assert(!contains(mapped,1), "{2,4,6}")
      assert(contains(mapped,2), "{2,4,6}")
      assert(!contains(mapped,3), "{2,4,6}")
      assert(contains(mapped,4), "{2,4,6}")
      assert(!contains(mapped,5), "{2,4,6}")
      assert(contains(mapped,6), "{2,4,6}")
      assert(!contains(mapped,7), "{2,4,6}")
      val mapped2 = map(union(s12,singletonSet(1000)), x => x - 2)
      assert(contains(mapped2,-1), "{-1,0,998}")
      assert(contains(mapped2,0), "{-1,0,998}")
      assert(!contains(mapped2,1), "{-1,0,998}")
      assert(!contains(mapped2,2), "{-1,0,998}")
      assert(contains(mapped2,998), "{-1,0,998}")
      assert(!contains(mapped2,1000), "{-1,0,998}")
    }
  }

}
