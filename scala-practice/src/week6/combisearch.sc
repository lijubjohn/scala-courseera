object pairs{
  val n =7

  1 until n map (i =>
     1 until n map (j => (i,j)))
}



def scalarProduct(xs:List[Double],ys:List[Double]) :Double = {

  (for((x,y) <- xs zip ys) yield x*y).sum
}