
def init[T](xs: List[T]):List[T] = xs match {
  case List() => throw new Error("init on empty list")
  case List(x) => List()
  case x:: xs => x::init(xs)
}


def remove [T](n :Int,xs :List[T]) : List[T] = (xs take n) ::: (xs drop n+1)


object merges {
  def mergeSort(xs: List[Int]): List[Int] = {
    var n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }

      val (fs, sd) = xs.splitAt(n)
      merge(mergeSort(fs), mergeSort(sd))
    }

  }

  val list = List(3,-1,20,2,30,-10,3,5)
  mergeSort(list)
  println(mergeSort(list))
}



