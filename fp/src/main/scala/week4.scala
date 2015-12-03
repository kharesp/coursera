package week4
trait List[T]{
	def isEmpty:Boolean
	def head:T
	def tail:List[T]
}

class Cons[T](val head:T,val tail:List[T]) extends List[T]{
	def isEmpty=false
}

class Nil[T] extends List[T]{
	def isEmpty: Boolean = true
	def head: Nothing = throw new NoSuchElementException("Nil.head")
	def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

/** Function values are essentially objects with apply methods. 
  * So the following object List with apply methods essentailly 
  * encodes methods List(),List(x) and List(x,y) */ 
object List
{
	def apply[T]():List[T] = new Nil
	def apply[T](elem1:T):List[T] = new Cons(elem1,new Nil)
	def apply[T](elem1:T,elem2:T): List[T] = 
		new Cons(elem1,new Cons(elem2,new Nil))
}

object Week4 extends App{
	def singleton[T](elem: T): List[T] = new Cons(elem, new Nil)
	def nth[T](index:Int,xs:List[T]):T =
		if (xs.isEmpty) throw new IndexOutOfBoundsException
		else if (index==0) xs.head 
		else nth(index-1,xs.tail)
	val list=new Cons(1,new Cons(2,new Cons(3,new Nil)))

	/** Function value f is mapped to an object with apply method. 
          * Example: 
          * val f = (x:Int) => x*x; f(7) maps to: 
	  * val f = new Function1[Int,Int]{
	  * 		def apply(x:Int):Int = x*x 
	  * }; f.apply(7) 
	  * Hence, List(1,2) is used as a function type and is equivalent to
	  * List.apply(1,2) */ 
	val list2=List(1,4)
	println(nth(1,list2))
}
