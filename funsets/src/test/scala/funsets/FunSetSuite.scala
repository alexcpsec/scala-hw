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
  test("singletonSet1 contains 1") {
    
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
    }
  }
  
  test("singletonSet1 does not contains 2") {
    new TestSets {
      assert(contains(s1, 2) === false, "Singleton(1) does not contain 2")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersection contains no elements") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(contains(s1, 1) && !contains(s, 1), "Intersect-1 1")
      assert(contains(s2, 2) && !contains(s, 2), "Intersect-1 2")
    }
  }
  
  test("intersection of a union is the set") {
    new TestSets {
      val s = union(s1, s2)
      val ss1 = intersect(s, s1)
      assert(contains(s1, 1) && contains(ss1, 1), "Intersect-2 1")
      assert(contains(s2, 2) && !contains(ss1, 2), "Intersect-2 2")
    }
  }
  
  test("diff union and intersection is union") {
    new TestSets {
      val su = union(s1, s2)
      val si = intersect(s1, s2)
      val s = diff(su, si)
      assert(contains(s, 1), "Diff-1 1")
      assert(contains(s, 2), "Diff-1 2")
    }
  }
  
  test("diff singletons is first set") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff-Singleton 1")
      assert(!contains(s, 2), "Diff-Singleton 2")
    }
  }
  
  test("filter singleton1 is greater than zero has 1") {
    new TestSets {
      val s = filter(s1, (x:Int) => (x > 0))
      assert(contains(s, 1), "Filter-Singleton>0 1")
      assert(!contains(s, 2), "Filter-Singleton>0 2")
    }
  }
 
  test("filter singleton1 is less than zero does not have 1") {
    new TestSets {
      val s = filter(s1, (x:Int) => (x < 0))
      assert(!contains(s, 1), "Filter-Singleton<>0")
    }
  }
  
   test("forall union{1,2,3} bigger than 0") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(forall(s, (x: Int) => (x > 0)), "Forall bigger than 0")
    }
  }
  
  test("forall union{1,2,3} NOT smaller than 3") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(!forall(s, (x: Int) => (x < 3)), "Forall not SMALLER than 3")
    }
  }

  test("exists union{1,2,3} bigger than 0") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(exists(s, (x: Int) => (x > 0)), "Exists bigger than 0")
    }
  }
  
  test("exists union{1,2,3} NOT smaller than 0") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(!exists(s, (x: Int) => (x < 0)), "Exists NOT smaller than 0")
    }
  }
  
  test("map union{1,2,3}, x +3 finds 4,5,6") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val smap = map(s, (x:Int) => x + 3)
      assert(contains(smap, 4), "map x+3 has 4")
      assert(contains(smap, 5), "map x+3 has 5")
      assert(contains(smap, 6), "map x+3 has 6")
      assert(!contains(smap, 1), "map x+3 DOES NOT HAVE 1")
    }
  }
  
  test("map union{1,2,3}, x * x finds 1,4,9") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val smap = map(s, (x:Int) => x * x)
      assert(contains(smap, 1), "map x*x has 1")
      assert(contains(smap, 4), "map x*x has 4")
      assert(contains(smap, 9), "map x*x has 9")
      assert(!contains(smap, 2), "map x*x DOES NOT HAVE 2")
    }
  }
  
}
