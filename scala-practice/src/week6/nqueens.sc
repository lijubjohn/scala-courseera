object nqueens {

  def queens(n: Int): Set[List[Int]] = {

    def placeQueen(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueen(k - 1)
          col <- 0 until n
          if (isSafe(col, queens))
        } yield col :: queens
    }

    placeQueen(n)

  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {

    val rows = queens.length

    val colWithRows = (rows - 1 to 0 by -1) zip queens

    colWithRows forall {
      case (r, c) => c != col && math.abs(col - c) != rows - r
    }
  }

  def show(queens :List[Int]) ={
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col,"X ").mkString
    "\n" + (lines mkString "\n")
  }
  queens(4) map show

}