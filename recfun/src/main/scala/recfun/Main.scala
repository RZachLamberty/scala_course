package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print {
          pascal(col, row) + " "
        }
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 && r == 0) {
      1
    } else if (r < 0 || r < c) {
      0
    } else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceWithExpectedParen(chars: List[Char], expectedParens: Int): Boolean = {
      if (chars.isEmpty) {
        true
      } else {
        if (chars.head == '(') {
          balanceWithExpectedParen(chars.tail, expectedParens + 1)
        } else if (chars.head == ')') {
          if (expectedParens < 1) {
            false
          } else {
            balanceWithExpectedParen(chars.tail, expectedParens - 1)
          }
        } else {
          balanceWithExpectedParen(chars.tail, expectedParens)
        }
      }
    }
    balanceWithExpectedParen(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else if (money <= 0 && !coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
