class C {
  def m() = ()
}

object Test {
  def main(args: Array[String]) {
    val c1 = new C
    val c2 = new C
    val c3 = c1
    println(c1.m _ == c1.m _)
    println(c1.m _ == c2.m _)
    println(c1.m _ == c3.m _)
    println(c3.m _ == c2.m _)
  }
}

// vim: set ts=4 sw=4 et:
