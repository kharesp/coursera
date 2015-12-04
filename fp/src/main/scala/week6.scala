package week6
object Week6 extends App
{
	def combinations[T](xs: List[T],ys: List[T]): List[(T,T)] = 
		xs flatMap (x => ys map (y => (x,y)))
	
	def scalarProduct(xs: List[Int], ys: List[Int]): Int =
		(xs zip ys).map{ case (x,y) => x*y}.sum
	
	def isPrime(n: Int): Boolean = (2 until n) forall (x => n%x != 0)

 	/** 
	  * Generate all pairs (i,j) where j<i<n and i+j is prime 
          * for a given constant n. 
	  */
	def primePairs(n: Int) = (1 until n) flatMap 
		(x => (1 until x) map (y => (x,y))) filter { case (l,r) => isPrime(l+r)}

	def primePairsWithFor(n: Int) = for {
		x <- 1 until n
		y <- 1 until x
		if isPrime(x+y)
	} yield (x,y)

	def scalarProductWithFor(xs: List[Int], ys: List[Int]): Int = 
		(for((x,y) <- xs zip ys) yield (x*y)).sum

 	println(scalarProductWithFor(List(1,2),List(2,3)))
}
