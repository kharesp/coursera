package week5
import scala.math.Ordering
object Week5 extends App
{
	def isort(xs: List[Int]): List[Int] = xs match {
		case List() => List() 
		case y::ys => insert(y, isort(ys))
	}
	def insert(x: Int, xs: List[Int]): List[Int] = xs match {
		case List() => List(x) 
		case y::ys => if (x<=y) x::xs else y::(insert(x,ys))
	}
	def init[T](xs: List[T]): List[T] = xs match {
		case List() => throw new Error("Can't take init of empty list") 
		case List(x) => List()
		case y::ys => y:: init(ys)
	}
	def last[T](xs: List[T]): T = xs match {
		case List() => throw new Error("can't take last of empty list") 
		case List(x) => x 
		case y::ys => last(ys) 
	}
	def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
		case List() => ys 
		case z::zs => z::concat(zs,ys)
	}
	def reverse[T](xs: List[T]): List[T] = xs match {
		case List() => xs
		case y::ys => reverse(ys) ++ List(y)
	}
	/** Also, we can define removeAt as: 
	  * def removeAt[T](xs: List[T], n: Int): List[T] =
	  * 		(xs take n) ++ (xs drop n+1)
	  */
	def removeAt[T](xs: List[T],n: Int): List[T] = xs match {
	 	case List() => xs 
		case y::ys => if (n==0) ys else y::removeAt(ys,n-1)
	}
	/**
	  * implicit type parameters 
	  * We don't have to pass the implicit type parameter. 
	  * It will be inferred by the scala compiler. 
	  */
	def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
		def n = xs.length/2 
		if (n==0) xs
		else {
		 	def merge(xs: List[T], ys: List[T]): List[T] = (xs,ys) match {
				case (Nil,ys) => ys
				case (xs,Nil) => xs
				case (x::xs1,y::ys1) => if (ord.lt(x,y)) x::merge(xs1,ys) else 
					y::merge(xs,ys1)
			}
			val (l,r) = xs splitAt n 
			merge(msort(l),msort(r))
		}
	}
	def map[T,U](xs: List[T])(f: T=>U): List[U] = xs match {
		case Nil => Nil
		case y::ys => f(y)::map(ys)(f)
	}
	def filter[T](xs: List[T])(f: T => Boolean): List[T] = xs match {
		case Nil => Nil
		case y::ys => if (f(y)) y::filter(ys)(f) else filter(ys)(f)
	}
	def filterNot[T](xs: List[T])(f: T=>Boolean): List[T] = xs match {
		case Nil => Nil 
		case x::xs1 => if (!f(x)) x::filterNot(xs1)(f) else filterNot(xs1)(f)
	}
	def partition[T](xs: List[T])(f: T=>Boolean): (List[T],List[T])= xs match {
		case Nil => (Nil,Nil)
		case x::xs1 => 
		{
			val (l,r)=partition(xs1)(f)
			if (f(x)) (x::l,r) else (l,x::r)
		}
	}
	def takeWhile[T](xs: List[T])(f: T=>Boolean): List[T] = xs match {
		case Nil => Nil 
		case x::xs1 => if (f(x)) x::takeWhile(xs1)(f) else Nil
	}		
	def dropWhile[T](xs: List[T])(f: T=> Boolean): List[T] = xs match {
		case Nil => Nil 
		case x::xs1 => if (f(x)) dropWhile(xs1)(f) else xs
	}
	def span[T](xs: List[T])(f: T => Boolean): (List[T],List[T]) = xs match {
		case Nil => (Nil,Nil)
		case x::xs1 => {
			val (l,r) = span(xs1)(f)
			if (f(x)) (x::l,r) else (Nil,xs)
		}
	}
	def pack[T](xs: List[T]): List[List[T]] = xs match {
		case Nil => Nil
		case x::xs1 => {
			val (l,r)= span(xs)(y => y==x) 	
		 	l::pack(r)
		}
	}
	def encode[T](xs: List[T]): List[(T,Int)] = 
			map(pack(xs))(l => (l.head,l.length))


}
