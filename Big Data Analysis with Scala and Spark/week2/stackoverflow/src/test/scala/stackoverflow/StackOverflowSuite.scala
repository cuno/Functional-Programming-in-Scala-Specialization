package stackoverflow

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite, ShouldMatchers}

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll with ShouldMatchers{


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("the median function works") {
    import stackoverflow.Helpers.median_2
    implicit def tuplelize(input: Array[Int]) = input map ((-1, _))

    median_2(Array(1, 5, 6, 3, 9, 8, 4, 2, 0, 7)) shouldBe 4
    median_2(Array(1, 5, 7, 3, 9, 4, 6)) shouldBe 5
    median_2(Array(17, 13, 101)) shouldBe 17
    median_2(Array(17, 13)) shouldBe 15
    median_2(Array(7)) shouldBe 7
  }

}
