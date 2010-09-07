import scala.events._

class C {
  imperative evt e1[Unit]
  imperative evt e2[Unit]
  evt e3 = e2 && causedBy(e1)
  
  def onE1() {
    println("e1 triggered")
    e2()
  }

  def onE2() {
    println("e2 triggred")
  }

  def onE3() {
    println("e3 triggered")
  }

  e1 += onE1 _
  e2 += onE2 _
  e3 += onE3 _
}

object Test {
  def main(args: Array[String]) {
    val c = new C
    c.e1()
    c.e2()
  } 
}

// vim: set ts=4 sw=4 et:
