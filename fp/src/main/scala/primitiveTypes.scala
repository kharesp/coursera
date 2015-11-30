package idealized.scala
abstract class MyBoolean
{
	def ifThenElse[T](te: => T,ee: => T): T
	
	def &&(x: => MyBoolean): MyBoolean= ifThenElse(x,False)
	def ||(x: => MyBoolean): MyBoolean= ifThenElse(True,x)
	def unary_! : MyBoolean = ifThenElse(False,True)
	def ==(x: MyBoolean): MyBoolean= ifThenElse(x,x.unary_!)
	def !=(x: MyBoolean): MyBoolean= ifThenElse(x.unary_!,x) 
        /** Assuming False < True */
	def <(x: MyBoolean): MyBoolean = ifThenElse(False,x)
}
object True extends MyBoolean
{
	def ifThenElse[T](te: => T,ee: => T): T= te 
}
object False extends MyBoolean
{
	def ifThenElse[T](te: => T,ee: => T): T= ee
}

/**
  * Peano numbers: Representing natural numbers using only a Zero value and a 
  * successor function.
  */
abstract class Nat
{
	def isZero: Boolean
	def predecessor: Nat
	def successor: Nat = new Succ(this) 
	def +(x: Nat): Nat
	def -(x: Nat): Nat
}
object Zero extends Nat
{
	def isZero:Boolean = true 
	def predecessor: Nat = throw new Error("0.predecessor")
	def +(x: Nat): Nat = x 
	def -(x: Nat): Nat = if (x.isZero) this  else throw new Error("negative number")
}
class Succ(n:Nat) extends Nat
{
	def isZero: Boolean = false
	def predecessor: Nat = n  
	def +(x: Nat): Nat = if (x.isZero) this else new Succ(this)+x.predecessor 
	/** 
	  * Or def +(x:Nat):Nat = new Succ(n+x)
          */
	def -(x: Nat): Nat = if (x.isZero) this else n - x.predecessor
}
