package reductions

import common._
import org.scalameter._

import scala.annotation._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {
  @tailrec
  def bracesSequential(chars: Array[Char], sliceFrom: Int, sliceUntil: Int, imbaL: Int = 0, imbaR: Int = 0): (Int, Int) = {
    if (sliceFrom == sliceUntil) (imbaL, imbaR)
    else if (chars(sliceFrom) == '(') bracesSequential(chars, sliceFrom + 1, sliceUntil, imbaL + 1, imbaR)
    else if (chars(sliceFrom) == ')')
      if (imbaL > 0) bracesSequential(chars, sliceFrom + 1, sliceUntil, imbaL - 1, imbaR)
      else bracesSequential(chars, sliceFrom + 1, sliceUntil, imbaL, imbaR + 1)
    else bracesSequential(chars, sliceFrom + 1, sliceUntil, imbaL, imbaR)
  }

  /** Returns `true` if the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = bracesSequential(chars, 0, chars length) == (0, 0)

  /** Returns `true` if the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(idx: Int, until: Int, sliceFrom: Int, sliceUntil: Int): (Int, Int) = bracesSequential(chars, idx, until)

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, -1, -1)
      else {
        val mid = (from + until) / 2
        val ((ll, lr), (rl, rr)) = parallel(reduce(from, mid), reduce(mid, until))
        if (ll > rr) ll - rr + rl -> lr else rl -> (rr - ll + lr)
      }
    }

    reduce(0, chars length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
