object Test
{
  imperative evt testEvent[Int]

	def main(args: Array[String])
	{
    // register 2 reactions
		testEvent += ((v: Int) => println("reaction 1"))
		testEvent += ((v: Int) => println("reaction 2: v = " + v))

    // register reaction 3, override registered reactions
    testEvent += ((v: Int, proceed: Int => Unit) => println("reaction 3 without proceed: v = " + v))

    // trigger event
		testEvent(3)
	}
}
