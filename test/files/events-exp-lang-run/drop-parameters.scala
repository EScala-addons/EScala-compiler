import scala.events._

class C {
  imperative evt e1[Int,Int]

  evt e2(x: Int) = e1(x,_)
  evt e3(x: Int) = e1(_,x)
  evt e4() = e1(_,_)
}

object Test {
  def main(args: Array[String]) {
    val o = new C

    o.e2 += { (x: Int) => println("e2 "+x) }
    o.e3 += { (x: Int) => println("e3 "+x) }
    o.e4 += { () => println("e4") }

    o.e1(1,2)
  }
}
