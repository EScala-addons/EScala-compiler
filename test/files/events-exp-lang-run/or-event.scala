import scala.events._

class C {
  imperative evt e1[Int,String]
  imperative evt e2[String,Int]

  evt e3[Int,String] = e1.map((n:Int,s:String)=>(n,s)) || e2.map((s:String,n:Int)=>(n,s))
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    o.e3 += { (x: Int,y: String) => println("e3 "+x+" "+y) }

    o.e1(1,"Hello")
    o.e2("World",2)

  }
}
