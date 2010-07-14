class C1 {
  def m(i: Int) = println("m(" + i + ")")
  evt e[Int] = beforeExec(m)

  e += react _

  def react(i: Int) = println("before m(" + i + ")")

}

class C2 extends C1 {
  override evt e[Int] = super.e
  observable override def m(i: Int) = {
    println("overriding observable method")
    super.m(i)
  }
}

object Test {
  def main(args: Array[String]) {
    val c = new C2
    c.e += react _
    c.m(4)
  }
  def react(i: Int) = println("Test reaction before: m(" + i + ")")
}

// vim: set ts=4 sw=4 et:
