object Test
{
  imperative evt e[Int]

	def main(args: Array[String])
	{
		val reaction2 = ((v: Int, proceed: Int => Unit) => println("reaction 2 overrides proceed, v = " + v))

    // register 3 reactions
		e += ((v: Int) => println("reaction 1, v = " + v))
		e += reaction2
		e += ((v: Int) => println("reaction 3"))

    // trigger event
		e(5, (v: Int) => println("trigger with v = " + v))

    // unregister reaction 2
    e -= reaction2

    // trigger event
    e(8, (v: Int) => println("trigger with v = " + v))
	}
}
