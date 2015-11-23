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

object week4 extends App{
	def singleton[T](elem: T): List[T] = new Cons[T](elem, new Nil[T])
	def nth[T](index:Int,xs:List[T]):T =
		if (xs.isEmpty) throw new IndexOutOfBoundsException
		else if (index==0) xs.head 
		else nth(index-1,xs.tail)
	val list=new Cons(1,new Cons(2,new Cons(3,new Nil)))
	println(nth(4,list))
}
