object IntSets{
  val t1 = new NonEmptySet(3,new EmptySet,new EmptySet)
  val t2 = t1 incl 4
}

abstract class IntSet{
  def incl(x:Int):IntSet
  def contains(x:Int):Boolean
  def union(other :IntSet):IntSet
}

class EmptySet extends IntSet{
  override def incl(x: Int): IntSet = new NonEmptySet(x,new EmptySet,new EmptySet)

  override def contains(x: Int): Boolean = false

  override def toString: String = "."

  override def union(other: IntSet): IntSet = other
}

case class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet{
  override def incl(x: Int): IntSet = {
    if (x < elem) new NonEmptySet(x,left incl x,right)
    else if (x > elem) new NonEmptySet(x,left ,right incl x)
    else this
  }

  override def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }


  override def union(other: IntSet): IntSet = ((left union right) union other) incl elem

  override def toString: String = "{" + left + elem + right + "}"
}