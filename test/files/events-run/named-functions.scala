class C {

  def m(i: Int) = println(i)
  def m2() = println("test")

  println(m _ == this.m _)

}

object Test {

  def main(args: Array[String]) {
    val c = new C
    println(c.m _ == c.m _)
    println(c.m _ == c.m2 _)

    val c2 = new C
    println(c.m _ == c2.m _)
    println(c.m _ == c2.m2 _)

  }
}

// vim: set ts=4 sw=4 et:
