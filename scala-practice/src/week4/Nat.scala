package week4

/**
  * Created by liju on 6/20/17.
  *
  * Review it again
  * Peano numbers
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat{
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new Error("0.predecessor")

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if(this.isZero) this else throw new Error("-ve number")
}

class Succ(n:Nat) extends Nat{
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if(that.isZero) this else (n - that.predecessor)
}