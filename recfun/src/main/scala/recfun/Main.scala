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
  def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    var i = 0
    def checkBalance(tmp: List[Char], x: Int): Boolean = {
      i = x
      if (i < 0) false
      else if (tmp.isEmpty) i == 0
      else if (tmp.head == '(') checkBalance(tmp.tail, i + 1)
      else if (tmp.head == ')') checkBalance(tmp.tail, i - 1)
      else checkBalance(tmp.tail, i)
    }
    checkBalance(chars, i)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def combinations(money: Int, coins: List[Int], ways: Int): Int = {
      if (money < 0) return ways
      if (coins.isEmpty) {
        if (money == 0) return ways + 1 else return ways
      }
      combinations(money, coins.tail, ways) + combinations(money - coins.head, coins, ways)
    }
    combinations(money, coins, 0)
  }
}
