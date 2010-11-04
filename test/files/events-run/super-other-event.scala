class C1 {
  evt e1[Unit] = afterExec(m)
  def m() = println("m")
  e1 += (() => println("e1()"))
}

class C2 extends C1 {
  override imperative evt e1[Unit]
  evt e2[Unit] = super.e1 || beforeExec(m)
  e2 += (() => println("e2()"))
}

object Test {
  def main(args: Array[String]) {
    val c1 = new C1
    val c2 = new C2
    println("on c1")
    println("m called")
    c1.m()
    println("on c2")
    println("m called")
    c2.m()
    println("imperative e1")
    c2.e1()
  } 
}

// vim: set ts=4 sw=4 et:
