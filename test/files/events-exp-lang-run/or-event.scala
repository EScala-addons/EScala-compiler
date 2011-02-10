import scala.events._

class C {
  imperative evt e1[Int,String]
  imperative evt e2[String,Int]

  evt e3(n: Int,s: String) = e1(n,s) || e2(s,n)
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    o.e3 += { (x: Int,y: String) => println("e3 "+x+" "+y) }

    o.e1(1,"Hello")
    o.e2("World",2)

  }
}
