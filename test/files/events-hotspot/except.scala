import scala.events._

object Test
{
  imperative evt e1[Int]
  imperative evt e2[Int]
  evt e3 = e1 || e2
  evt e4 = e3 \ e1

	def main(args: Array[String])
	{
    // register reactions to event 1 and 2
    e1 += ((v: Int) => println("reaction of event 1, v = " + v))
    e2 += ((v: Int) => println("reaction of event 2, v = " + v))

    // register reaction to event 3 and 4
    e3 += ((v: Int, proceed: Int => Unit) => println("reaction of e3 = e1 || e2 override proceed, v = " + v))
    e4 += ((v: Int, proceed: Int => Unit) =>
      {
        println("e4 = e3 \\ e1")
        println("reaction of event 4, v = " + v)
        println("now call proceed")
        proceed(v)
      })

    // trigger event 2
		e2(1, (v: Int) => println("trigger event 2 with v = " + v))

    // trigger event 1
		e1(2, (v: Int) => println("trigger event 1 with v = " + v))

    // trigger event 2
		e2(3, (v: Int) => println("trigger event 2 with v = " + v))
	}
}
