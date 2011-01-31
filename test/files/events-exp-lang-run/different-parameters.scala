import scala.events._

class C {
  imperative evt e1[Int,String]

  evt e2(x: Int, y: String) = e1(x,y)
  evt e3(x: String, y: Int) = e1(y,x)
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    o.e2 += { (x: Int,y: String) => println("e2 "+x+" "+y) }
    o.e3 += { (x: String,y: Int) => println("e3 "+x+" "+y) }

    o.e1(1,"event1")
  }
}
