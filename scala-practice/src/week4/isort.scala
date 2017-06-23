package week4

/**
  * Created by liju on 6/20/17.
  */
class isort {

  def isort(xs:List[Int]):List[Int] = xs match {
    case List() => List()
    case y::ys => insert(y,isort(ys))
  }

  def insert(x:Int,xs:List[Int]):List[Int] = xs match {
    case List() => List(x)
    case y::ys => if (x <= y ) x::xs else  y:: insert(x,ys)
  }

}
