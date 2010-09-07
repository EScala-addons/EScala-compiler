import scala.events._
class C {
  imperative evt e1[Unit]
  imperative evt e2[Unit]
  evt e3 = e1 || e2
  evt e4 = e3 && causedBy(e1)
  e3 += {() => println("e3 triggered")}
  e4 += {() => println("e4 triggered")}
}

object Test {
  def main(args: Array[String]) {
    val c = new C
    c.e1()
    c.e2()
  } 
}

// vim: set ts=4 sw=4 et:
