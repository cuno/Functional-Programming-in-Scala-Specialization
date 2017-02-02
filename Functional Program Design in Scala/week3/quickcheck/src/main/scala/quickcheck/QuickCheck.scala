package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency(
      (9, genHeap),
      (1, Gen.const(empty))
    )
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOfInsertToEmptyHeap") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * Hint 1.
    * If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the
    * smallest of the two elements back.
    */
  property("minOfAdding2ToEmptyHeap") = forAll { (a: A, b: A) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (a min b)
  }

  /**
    * Hint 2.
    * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
    */
  property("addToEmptyHeapAndDeleteMinimum") = forAll { a: A => isEmpty(deleteMin(insert(a, empty))) }

  /**
    * Hint 3.
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("sortedSequence") = forAll { h: H =>
    def sorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val prevMinimum = findMin(h)
        val h2 = deleteMin(h)
        // The previous minimum must always be smaller than or equal to the current minimum.
        isEmpty(h2) || prevMinimum <= findMin(h2) && sorted(h2)
      }

    sorted(h)
  }

  /**
    * Hint 4.
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  property("minimumOfTwoHeapMeld") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    findMin(h) == (m1 min m2)
  }

  /**
    * The melding of two heaps where the minimum of one heap is moved to the other heap should return the
    * same heap as only melding the two heaps.
    */
  property("meldOfTwoHeapsWithMovedMinimum") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

}
