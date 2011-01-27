import scala.events._

class C {
  imperative evt e1[Int,Int]

  evt e2(x: Int, y: Int) = e1(x,y)
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    o.e2 += { (x: Int,y: Int) => println("e2 "+x+" "+y) }
    o.e1(1,2)
  }
}
