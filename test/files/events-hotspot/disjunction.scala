object Test
{
  imperative evt e1[Int]
  imperative evt e2[Int]
  evt e3 = e1 || e2

	def main(args: Array[String])
	{
    // register reactions to event 1 and 2
    e1 += ((v: Int) => println("event 1 reaction 1"))
		e1 += ((v: Int, proceed: Int => Unit) => { println("event 1 reaction 2 with proceed"); proceed(v) })
    e2 += ((v: Int) => println("event 2 reaction 1"))
		e2 += ((v: Int) => println("event 2 reaction 2"))

    // register reaction to event 3
    e3 += ((v: Int, proceed: Int => Unit) =>
      {
        print("e3 = e1 || e2 reaction calls proceed = ")
        proceed(v)
      })

    // trigger event 2
		e2(2, (v: Int) => println("trigger event 2"))

    // trigger event 1
		e1(3, (v: Int) => println("trigger event 1"))
	}
}
