import scala.events._

class A {
  imperative evt e1[Int, String]

  evt e2(a: Int, b: String) = m()(a, b)

  def m() = {e1}
}

object Test {
  def main(args: Array[String]) {
    val o = new A
    
    o.e2 += { (a: Int, b: String) => println("e2") }

    o.e1(1337, "me")
  }
}
