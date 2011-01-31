import scala.events._

object C1 {
  imperative evt e1[Unit]
}

class C2 {
  evt e1() = C1.e1

  e1 += { () => println("C2.e1") }
}
  

object Test {
  def main(args: Array[String]) {
    val o = new C2
    C1.e1()
  }
}
