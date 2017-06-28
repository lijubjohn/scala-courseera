object polynomials {

  class Poly(val terms:Map[Int,Double]){
    def + (that : Poly):Poly = new Poly(terms ++ (that.terms map adjust))

    def adjust(term:(Int,Double)) :(Int,Double) ={
      val (exp,coef) = term
      terms get exp match {
        case Some(coef1) => exp -> (coef+coef1)
        case None=> exp -> coef
      }
    }



    override def toString: String = (for((exp,coef)<- terms.toList.sorted.reverse) yield (coef + "x^" +exp )) mkString "+"
  }

  val pol1 = new Poly(Map(1->2.0, 3->4.0, 5->6.2))
  val pol2 = new Poly(Map(0->3.0,3->7.0))
  val sum = pol1 + pol2
}