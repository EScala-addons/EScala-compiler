import scala.events._

class C {
  imperative evt e1[Unit]
  imperative evt e2[Unit]
  imperative evt e3[Unit]
  evt e4 = e3 within(between(e1, e2))
}

object Test {
  def main(args: Array[String]) {
    val c = new C
    c.e1 += {() => println("e1")}
    c.e2 += {() => println("e2")}
    c.e3 += {() => println("e3")}
    c.e4 += {() => println("e4")}

    c.e1()
    c.e2()
    c.e3()
    c.e1()
    c.e3()
    c.e2()
    c.e3()
  }
}

// vim: set ts=2 sw=2 et:
