import scala.events._

class C {
  imperative evt e1[Unit]

  evt e2() = e1()
  evt e3() = e1
  evt e4 = e1
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    o.e2 += { () => println("e2") }
    o.e3 += { () => println("e3") }
    o.e4 += { () => println("e4") }
    o.e1()
  }
}
