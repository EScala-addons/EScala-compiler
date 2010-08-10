import scala.events._
class C {
  imperative evt e1[Unit]
  evt e2 = e1 && within(execution(m))
  observable def m(i: Int, s: String) {
    e1()
    println("m(" + i + ", " + s + ")")
  }
}

object Test {
  def main(args: Array[String]) {
    val c = new C
    c.e1 += {() => println("e1")}
    c.e2 += {() => println("e2")}
    c.e1()
    c.m(1,"s")
  }
}

// vim: set ts=2 sw=2 et:
