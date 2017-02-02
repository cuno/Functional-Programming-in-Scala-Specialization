package funsets

import org.scalatest.FunSuite

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
//@RunWith(classOf[JUnitRunner])
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
    * val s1 = singletonSet(1)
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

  test("intersect contains only the elements that are in both sets") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val si2 = intersect(s12, s23)
      assert(!contains(si2, 1), "Intersect 1")
      assert(contains(si2, 2), "Intersect 2")
      assert(!contains(si2, 3), "Intersect 3")
    }
  }

  test("diff contains only the elements that are in the first but not in the second set") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val sd1 = diff(s12, s23)
      assert(contains(sd1, 1), "Diff 1")
      assert(!contains(sd1, 2), "Diff 2")
      assert(!contains(sd1, 3), "Diff 3")
    }
  }

  test("xor contains only the elements that are not in both sets") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val sx1 = xor(s12, s23)
      assert(contains(sx1, 1), "Diff 1")
      assert(!contains(sx1, 2), "Diff 2")
      assert(contains(sx1, 3), "Diff 3")
    }
  }

  test("filter contains only the elements of a set that are accepted by a given predicate") {
    new TestSets {
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)
      val sf3 = filter(s123, i => i > 2)
      val sf23 = filter(s123, i => i > 1)
      val sf123 = filter(s123, i => i > 0)
      val sfempty = filter(s123, i => false)

      assert(contains(s123, 1), "123 contains 1")
      assert(contains(s123, 2), "123 contains 2")
      assert(contains(s123, 3), "123 contains 3")

      assert(!contains(sf3, 1), "Filter 1a")
      assert(!contains(sf3, 2), "Filter 2a")
      assert(contains(sf3, 3), "Filter 3a")

      assert(!contains(sf23, 1), "Filter 1b")
      assert(contains(sf23, 2), "Filter 2b")
      assert(contains(sf23, 3), "Filter 3b")

      assert(contains(sf123, 1), "Filter 1c")
      assert(contains(sf123, 2), "Filter 2c")
      assert(contains(sf123, 3), "Filter 3c")
      assert(!contains(sf123, 4), "Filter 4c")

      assert(!contains(sfempty, 1), "Filter 1d")
      assert(!contains(sfempty, 2), "Filter 2d")
      assert(!contains(sfempty, 3), "Filter 3d")
      assert(!contains(sfempty, 4), "Filter 4d")
    }
  }

  test("forall tests the predicate for all elements of the set") {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val sm10 = singletonSet(-10)
    val s1to5 = union(s1, union(s2, union(s3, union(s4, union(s5, sm10)))))

    assert(!contains(s1to5, 0), "s1to5 !contains 0")
    assert(contains(s1to5, 1), "s1to5 !contains 1")
    assert(contains(s1to5, 2), "s1to5 contains 2")
    assert(contains(s1to5, 3), "s1to5 contains 3")
    assert(contains(s1to5, 4), "s1to5 contains 4")
    assert(contains(s1to5, 5), "s1to5 contains 5")
    assert(!contains(s1to5, 6), "s1to5 !contains 6")

    assert(forall(s1to5, x => x >= -10 & x != 100), "Forall 1")
  }

  test("contains works") {
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      assert(exists(s123, i => i > 2))
      assert(exists(s123, i => i < 3))
      assert(!exists(s123, i => i > 3))
      assert(!exists(s123, i => i < 1))
    }
  }

  test("map works") {
    import FunSets.{toString => ts}
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      assert(ts(map(s123, x => x)) === "{1,2,3}")
      assert(ts(map(s123, _ * 3)) === "{3,6,9}")
      assert(ts(map(s123, _ / 2)) === "{0,1}")
      assert(ts(map(s123, x => -x)) === "{-3,-2,-1}") // I blame toString :P
    }
  }

}