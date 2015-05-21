package recfun
import common._

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
    if(c == 0 || c == r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balances(chars: List[Char], count: Int): Boolean = {
      if(chars.isEmpty) count == 0
      else if (chars.head == '(') balances(chars.tail, count+1)
      else if (chars.head == ')') count >0 && balances(chars.tail, count-1)
      else balances(chars.tail, count)
    }
    balances(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(money: Int, coins: List[Int]):Int = {
      if (coins.isEmpty) 0
      else if (money - coins.head == 0) 1
      else if (money - coins.head < 0) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
    count(money, coins.sorted)
  }
}
