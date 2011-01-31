import scala.events._

class C {
  imperative evt e1[String, Boolean, Int, String, Boolean, Int]

  evt e2(s1: String, s2: String, x: Int, y: Int) = e1(s1,_,y,s2,_,x)
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    o.e2 += { (s1: String, s2: String, x: Int,y: Int) => println("e2 "+s1+" "+s2+" "+x+" "+y) }
    o.e1("Hello", true, 2, "World", false, 4)
  }
}
