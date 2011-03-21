object Test
{
  imperative evt e1[Int]
  imperative evt e2[Int]
  imperative evt e3[Int]
  evt e4 = e1 then e2
  evt e5 = e2 then e1
  evt e6 = e1 then (e3, (v1:Int, v2:Int) => v1 + v2)

	def main(args: Array[String])
	{
    // register reactions to event 1, 2 and 3
    e1 += ((v: Int) => println("reaction of event 1"))
    e2 += ((v: Int) => println("reaction of event 2"))
    e3 += ((v: Int, proceed: Int => Unit) => println("reaction of event 3 override proceed, v = " + v))

    // register reaction to event 4, 5 and 6
    e4 += ((v: (Int,Int), proceed: ((Int,Int)) => Unit) =>
      {
        println("e4 = e1 then e2 calls proceed first")
        proceed(v)
        println("reaction of event 4, v1 = " + v._1 + "   v2 = " + v._2)
      })
    e5 += ((v: (Int,Int), proceed: ((Int,Int)) => Unit) =>
      {
        println("e5 = e2 then e1 dosn't use proceed")
        println("reaction of event 5, v1 = " + v._1 + "   v2 = " + v._2)
      })
    e6 += ((v: Int, proceed: (Int) => Unit) =>
      {
        println("e6 = e1 then e3")
        println("reaction of event 6, v = " + v)
        println("now call proceed")
        proceed(v)
      })

    // trigger event 2
		e2(0, (v: Int) => println("trigger event 2 with v = " + v))

    // trigger event 1
		e1(1, (v: Int) => println("trigger event 1 with v = " + v))

    // trigger event 2
		e2(2, (v: Int) => println("trigger event 2 with v = " + v))

    // trigger event 3
		e3(3, (v: Int) => println("trigger event 3 with v = " + v))
	}
}
