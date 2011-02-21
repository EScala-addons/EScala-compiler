package scala.events.test.recursive
import scala.events._

object Test {
	
	val Ev1 = new ImperativeEvent[Unit]
	val Ev2 = new ImperativeEvent[Unit]
	
	/* this does not work, throws NullPointerException at Initialisation
	val state1 = between(Ev1 || (Ev2 within state2), Ev2)
	val state2 : IntervalEvent[Unit] = between((Ev2 \ state1.before) within state1, Ev2)
	*/
	//this works
	//note that Ev2 and state1.after works while Ev2 within state1 does not
	val state1 = Lazy(between(Ev1 || (Ev2 within state2), Ev2 and state2.before))
	val state2 : IntervalEvent[Unit] = between((Ev2 \ state1.before) within state1, Ev2)
	/* note that this definition would be better off using state2 = between(Ev2 and state1.after,Ev2)
	 * this is just to show that the recursive definition works
	 * Something that btw. also works is to put a EventNodeLazyExcept(Ev2, state2.before) as the first
	 * parameter of the lazy_Within
	 */
	
	def main(args: Array[String]) = {
		state1.before += {_ => println("before state1")}
		state1.after += {_=> println("after state1")}
		state2.before += {_=> println("before state2")}
		state2.after += {_=> println("after state2")}
		Ev1 += {_=> println("Event")}
		Ev2 += {_=> println("Event")}
		
		Ev1()
		println("---")
		Ev2()
		println("---")
		Ev2()
		println("---")
		Ev2()
		println("---")
		Ev2()
	}
	
}