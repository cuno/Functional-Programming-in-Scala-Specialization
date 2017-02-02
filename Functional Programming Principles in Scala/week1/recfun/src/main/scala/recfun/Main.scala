package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 | c == r) 1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]) = {
    @tailrec
    def bal(chars: List[Char], stackSize: Int): Boolean = chars match {
      case '(' :: tail =>
        bal(tail, stackSize + 1)
      case ')' :: tail =>
        if (stackSize == 0) false else bal(tail, stackSize - 1)
      case Nil => stackSize == 0
    }

    bal(chars filter (c => c == '(' | c == ')'), 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]) = {
    def find(coins: List[Int], sum: Int): Int = {
      coins match {
        case Nil => 0
        case coin :: rCoins if sum < 0 => 0
        case coins if sum == 0 => 1
        case coins => find(coins, sum - coins.head) + find(coins tail, sum)
      }

    }
    if (money == 0) 1 else find(coins, money)
  }
}
