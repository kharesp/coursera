trait Expr
{
	def eval: Int = this match {
		case Number(n) => n 
		case Sum(e1,e2) => e1.eval + e2.eval 
		case Prod(e1,e2) => e1.eval * e2.eval
	}
	def show: String = this match {
		case Number(n) => n.toString 
		case Var(name) => name
		case Sum(e1, e2) => e1.show + " + " + e2.show
		case Prod(Sum(x,y),Sum(m,n)) => "("+x.show + " + " + y.show +")" + 
			" * " + "("+m.show + " + " + n.show +")"
		case Prod(Sum(x,y),z) => "("+x.show + " + " + y.show +")"+ " * " + z.show
		case Prod(z,Sum(x,y)) => z.show + " * " + "("+x.show + " + " + y.show +")"
	}
}
case class Number(n: Int) extends Expr
case class Sum(e1: Expr,e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(name: String) extends Expr

object Test extends App
{
	val e = Sum(Sum(Number(1),Number(2)),Sum(Number(1),Number(2)))
	println(e.show)
}
