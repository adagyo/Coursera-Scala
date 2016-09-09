package recfun

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
    if (c == 0 || c == r) 1
    else if (c == 1 || c == r-1) r
    else pascal(c, r-1) + pascal (c-1, r-1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countModifier(car: Char): Int = {
      if(car == '(')  1
      else if(car == ')') -1
      else 0
    }
    def balanceIter(count: Int, s: List[Char]): Boolean = {
        if(count < 0) return false
        if(s.isEmpty) return count == 0
        balanceIter(count + countModifier(s.head), s.tail)
    }
    balanceIter(0,chars)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countIter(currentCoins: List[Int], nbCoins: Int, currentMoney: Int): Int = {
      if(currentMoney == 0) return 1
      if(currentMoney < 0 || nbCoins <= 0) return 0

      countIter(currentCoins, nbCoins - 1, currentMoney) +
        countIter(currentCoins, nbCoins, currentMoney - currentCoins.last)
    }
    countIter(coins,coins.length,money)
  }
}
