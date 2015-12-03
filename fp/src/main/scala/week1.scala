package week1
object Week1 extends App{
	/** Non-terminating expression */
	def loop:Boolean= loop 
	
	/** Value definition by name
	  * RHS is evaluated every time the variable is used 
	  */
	def y:Boolean= loop
	
	/** Value definition by value. 
	  * Value is only evaluated once at the time of definition
	  * Following will not terminate:
	  * val x:Boolean=loop 
	  */
	
	/** Function arguments can either be passed by value(default)
	  * or by name. Arguments passed by value are evaluted first 
	  * before the function is evaluated. If arguments are passed 
	  * by name, the function is applied to un-reduced arguments. 
	  * => is used to denote that an argument is passed by name
	  */
	
	/** Passing second function argument by name */
	def and(x:Boolean,y: => Boolean):Boolean = if (x) y else false 
	def or(x:Boolean,y: => Boolean):Boolean = if(x) true else y
	
	/** As second function argument is passed by name, the following 
	  * expression gets evaluated else it would not terminate.
	  */
	val result = and(false,loop) 
	
	def abs(x:Double):Double = if (x>=0) x else -x 
	
	def sqrt(x:Double):Double = {
		def isGoodEnough(guess:Double):Boolean = 
			abs(guess*guess - x)/x < 0.001
	
		def improve(guess:Double):Double = 
			(guess + x/guess)/2 
	
		def sqrtIter(guess:Double):Double = 
			if (isGoodEnough(guess)) guess 
			else sqrtIter(improve(guess))
	
		sqrtIter(1.0)
	}
	println("Square root of 2:"+sqrt(2))
	
	/** Tail Recursive gcd (greatest common divisor) function */
	def gcd(a:Int,b:Int):Int= if(b==0) a else gcd(b,a%b)
	
	println("GCD of 21,14 is:"+gcd(21,14))
	
	/** Non Tail recursive factorial function */
	def factorial(n:Int):Int= if(n==0) 1 else n*factorial(n-1)
	
	/** Tail recursive factorial function */
	def tailRecursiveFactorial(n:Int):Int={
		def loop(acc:Int,n:Int):Int =
			if(n==0) acc 
			else loop(acc*n,n-1)
		loop(1,n)
	}
	println("factorial of 5:"+tailRecursiveFactorial(5))

}
