object Exercise{
  def product(f:Int=>Int)(a:Int,b:Int):Int = {
      if (a > b) 1
      else f(a) * product(f)(a+1,b)
  }

  product(x=>x*x)(3,4)

  def fact(n:Int) = product(x=>x)(1,n)
  fact(5)


  def mapreduce(f:Int=> Int,combine:(Int,Int)=> Int,zero : Int)(a:Int,b:Int):Int ={
    if (a>b) zero
    else
      combine(f(a),mapreduce(f,combine,zero)(a+1,b))
  }

  def productt(f:Int=> Int)(a:Int,b:Int):Int = mapreduce(f,(x,y)=>x*y,1)(a,b)


  def factt(n:Int) = product(x=>x)(1,n)
  factt(5)
}
