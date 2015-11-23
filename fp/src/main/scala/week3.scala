abstract class IntSet{
	def incl(x:Int):IntSet
	def contains(x:Int):Boolean
	def union(other:IntSet):IntSet
}

object Empty extends IntSet{
	def incl(x:Int):IntSet=new NonEmpty(x,Empty,Empty)
	def contains(x:Int):Boolean=false
	override def toString = "."
	def union(other:IntSet):IntSet = other
}

class NonEmpty(elem:Int,left:IntSet,right:IntSet) extends IntSet{
	def contains(x:Int):Boolean=
		if(x<elem) left contains x 
		else if (x>elem) right contains x
		else true
	def incl(x:Int):IntSet=
		if(x<elem) new NonEmpty(elem,left incl x,right)
		else if (x>elem) new NonEmpty(elem,left,right incl x)
		else this
	override def toString = "{" + left + elem + right + "}"
	
	def union(other:IntSet):IntSet = ((left union right) union other) incl elem
}

object week3 extends App{
	val tree= new NonEmpty(3, Empty, Empty).incl(4)
	val other=new NonEmpty(2,Empty,Empty).incl(5)
        val newTree=tree union other	
	println("Binary tree: "+ newTree)
}