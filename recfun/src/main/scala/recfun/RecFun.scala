package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], openParenCount: Int): Boolean =
      if (chars.isEmpty) {
        if (openParenCount == 0) true
        else false
      } else if (openParenCount < 0) false
      else {
        if (chars.head == '(') loop(chars.tail, openParenCount + 1)
        else if (chars.head == ')') loop(chars.tail, openParenCount - 1)
        else loop(chars.tail, openParenCount)
      }

    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def computeCombinations(money: Int, coins: List[Int], combinations: Int): Int =
      if (money == 0 || coins.isEmpty) combinations
      else applyCoin(money, coins.head, coins.tail, combinations)

    def applyCoin(money: Int, coin: Int, remainingCoins: List[Int], combinations: Int): Int =
      (
        if (money - coin == 0) combinations + 1
        else if (money - coin < 0) combinations
        else applyCoin(money - coin, coin, remainingCoins, combinations)
        ) + computeCombinations(money, remainingCoins, combinations)

    computeCombinations(money, coins, 0)
  }
}
