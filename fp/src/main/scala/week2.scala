object week2 extends App{
	
	/** Higher-Order Function accepting another function */
	def sum(f:Int=>Int,a:Int,b:Int):Int={
		def loop(acc:Int,n:Int):Int=
			if(n>b) acc else loop(f(n)+acc,n+1)
		loop(0,a)
	}
	println("Sum of integers from 1 to 5: "+sum(x=>x,1,5))
	
	/** Curried notation of function returning another function */
	def sumCurried(f:Int=>Int)(a:Int,b:Int):Int=
		if(a>b) 0 else f(a)+sumCurried(f)(a+1,b)
	sumCurried(x=>x)(1,5)
	println("Sum of integers from 1 to 5: "+sumCurried(x=>x)(1,5))
	
	/** Curried mapreduce function */
	def mapreduce(map:Int=>Int,reduce:(Int,Int)=>Int,zero:Int)(a:Int,b:Int):Int=
		if(a>b) zero 
		else reduce(map(a),mapreduce(map,reduce,zero)(a+1,b))
	
	/** Define product in terms of mapreduce */
	def product(f:Int=>Int)(a:Int,b:Int):Int=
		mapreduce(f,(x,y)=>x*y,1)(a,b)
	println("Product of integers from 1 to 5: "+product(x=>x)(1,5))

	def abs(x:Double):Double=
		if (x<0) -x else x

	/** Fixed point of a function */
	def fixedPoint(f:Double=>Double)(init:Double)={
		val tolerance:Double=0.0001
		def closeEnough(x:Double,y:Double):Boolean=
			abs(x-y)/x<tolerance 
		def iterate(guess:Double):Double={
			val next=f(guess)
			if (closeEnough(guess,next)) next 
			else iterate(next)
		}
		iterate(init)
	}
	println("Fixed point of f(x)=1+x/2: "+fixedPoint(x=>1+x/2)(1))

	def averageDamp(f:Double=>Double)(x:Double)=(x+f(x))/2	

	/** sqrt function in terms of fixed point */
	def sqrt(x:Double):Double=fixedPoint(averageDamp(y=>x/y))(1)
	println("Sqrt of 2: "+sqrt(2))

	val x=new Rational(1,3)
	val y=new Rational(5,7)
	val z=new Rational(3,2)
	println("Rational x-y-z: "+(x - y - z))
}
class Rational(x:Int,y:Int){
	require(y!=0,"Denominator should be non-zero")

	private def gcd(x:Int,y:Int):Int=
		if (y==0) x else gcd(y,x%y)
	val numer=x/gcd(x,y)
	val denom=y/gcd(x,y)
	def this(x:Int) = this(x,1)

	def +(that:Rational):Rational=
		new Rational(
			numer*that.denom + denom*that.numer,
			denom*that.denom)
	def unary_- :Rational= new Rational(-numer,denom)
	def -(that:Rational):Rational=this + -that 
	def <(that:Rational):Boolean= numer*that.denom < that.numer*denom
	def max(that:Rational):Rational= if(this<that) that else this
	override def toString()=numer+"/"+denom	
}
