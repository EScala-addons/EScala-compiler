import scala.events._

class C {
  imperative evt e1[Int,String]
  imperative evt e2[String,Int]
  imperative evt e3[Int]

  evt e4(n: Int,s: String) = e1(n,s) || e2(s,n)

  evt e5(n: Int) = e1(n,_) || e3(n)

  evt e6() = e1(_,_) || e2(_,_) || e3(_)
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    o.e4 += { (x: Int,y: String) => println("e4 "+x+" "+y) }
    o.e5 += { (x: Int) => println("e5 "+x) }
    o.e6 += { () => println("e6") }

    o.e1(1,"Hello")
    o.e2("World",2)
    o.e3(3)

  }
}
