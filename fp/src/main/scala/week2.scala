/** Higher-Order Function accepting another function */
def sum(f:Int=>Int,a:Int,b:Int):Int={
	def loop(acc:Int,n:Int):Int=
		if(n>b) acc else loop(f(n)+acc,n+1)
	loop(0,a)
}
sum(x=>x,1,5)

/** function returning another function */
def sumReturningFunction(f:Int=>Int):(a:Int,b:Int)=>Int={
	def sumF(a:Int,b:Int):Int=
		if(a>b) 0 else f(a)+sumF(a+1,b)
	sumF
}
sumReturningFunction(x=>x)(1,5)

/** Curried notation of function returning another function */
def sumCurried(f:Int=>Int)(a:Int,b:Int):Int=
	if(a>b) 0 else f(a)+sum(f)(a+1,b)
sumCurried(x=>x)(1,5)


/** Curried mapreduce function */
def mapreduce(map:Int=>Int,reduce:(Int,Int)=>Int,zero:Int)(a:Int,b:Int):Int=
	if(a>b) zero 
	else reduce(map(a),mapreduce(map,reduce,zero)(a+1,b))

/** Define product in terms of mapreduce */
def product(f:Int=>Int)(a:Int,b:Int):Int=
	mapreduce(f,(x:Int,y:Int):Int=>x*y,1)(a,b)
