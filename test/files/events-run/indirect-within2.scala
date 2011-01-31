package tests.indirect
import scala.events._


object Test{
	
	val ev = new ImperativeEvent[Unit]
	val betw = between(ev,ev)
	
	betw.before += (_ => println("Before"))
	betw.after += (_ => println("After"))
	
	val Vev1 = Variable(ev)
	val Vev2 = Variable(ev)
	val betw2 = between(Vev1.event(i => i),Vev2.event(i => i))
	betw2.before += (_ => println("Var before"))
	betw2.after += (_ => println("Var after"))
	Vev1 := null
	Vev1 := ev
	
	def main(args:Array[String]){
		ev() 
		/* this should either print Before and Var before, or
		 * Before, After, Var before, Var after (in whatever order)
		 * anything else is inconsistent 
		 */
	}
	
}