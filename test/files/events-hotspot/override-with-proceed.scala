object Test
{
  imperative evt testEvent[Int]

	def main(args: Array[String])
	{
		val reaction2 = ((v: Int) => println("reaction 2: v = " + v))

    // register 3 reactions
		testEvent += ((v: Int, proceed: Int => Unit) => { print("reaction 1 with 'proceed': "); proceed(v) })
		testEvent += reaction2
		testEvent += ((v: Int) => println("reaction 3"))

    // trigger event
		testEvent(5, (v: Int) => println("trigger with value 5"))

    // unregister reaction 2, register reaction 4
    testEvent -= reaction2
    testEvent += ((v: Int, proceed: Int => Unit) => { println("reaction 4: v = " + v); proceed(v) })

    // trigger event
    testEvent(8, (v: Int) => println("trigger with value 8"))
	}
}
