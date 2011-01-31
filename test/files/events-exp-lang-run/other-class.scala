import scala.events._

object O {
  object O_inner {
    imperative evt e1[Unit]
  }
}


class C {
  evt e1() = O.O_inner.e1()

  e1 += { () => println("C.e1") }
}
  

object Test {
  def main(args: Array[String]) {
    val o = new C
    O.O_inner.e1()
  }
}
