package variance

/** List is covariant */
trait List[+T]
{
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
	def prepend[U >: T] (elem: U) : List[U] = new Cons(elem, this)
}

object Nil extends List[Nothing]
{
	def isEmpty = true
	def head: Nothing = throw new NoSuchElementException("Nil.head") 
	def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]
{
	def isEmpty = false
}

object test extends App
{
	/** We can assign a List[Nothing] to List[String], as Nothing is a subtype of 
          * of String and Lists are covariant */
	val a: List[String] = Nil 
}
