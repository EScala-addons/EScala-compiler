class C1 {
  def m(i: Int) = println("m(" + i + ")")
  evt e[Int] = beforeExec(m)

  e += react _

  def react(i: Int) = println("before m(" + i + ")")

}

class C2 extends C1 {
  override def m(i: Int) = {
    println("overriding method")
    super.m(i)
  }
}

object Test {
  def main(args: Array[String]) {
    val c = new C2
    c.m(4)
  }
}

// vim: set ts=4 sw=4 et:
