def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => (y*y) :: squareList(ys)
  }

def squareList2 ()
/*
def squareList(xs: List[Int]): List[Int] =
  xs map ???*/


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
     val (first,rest) = xs1 span (y => x == y)
    first :: pack(rest)
}

def encode[T]( xs :List[T]):List[(T,Int)] = {
  pack(xs).map(x => (x.head,x.length))
}