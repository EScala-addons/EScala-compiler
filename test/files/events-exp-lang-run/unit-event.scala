import scala.events._

class C {
  imperative evt e1[Unit]

  evt e2() = e1()
}

object Test {
  def main(args: Array[String]) {
    val o = new C
    o.e2 += { () => println("e2") }
    o.e1()
  }
}
