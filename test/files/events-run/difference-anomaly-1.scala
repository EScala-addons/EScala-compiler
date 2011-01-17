import scala.events._

/*
 * This test shows a bug concerning AND and DIFFERENCE, occurring if the difference
 * collects reactions not knowing if they are needed, causing the AND-Node to think
 * one of it's sub-events occurred (while it did not), leading to collection of
 * reactions when the second event occurs
 */
object Test {

imperative evt event[Unit]

evt dummy1 = event || emptyevent
evt dummy2 = event || emptyevent

val dummyfkt = (a : Any) => { }

//deploy in the most disadvantegous manner
dummy1 += dummyfkt
dummy2 += dummyfkt

evt buggy = event and (dummy1 \ dummy2) //can never happen

buggy += (_ => println("Bug"))

 def main(args: Array[String]) {
  event() // this should NOT cause any reaction
 }





}