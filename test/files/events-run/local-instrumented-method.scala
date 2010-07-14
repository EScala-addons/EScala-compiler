class C {
  def m(i: Int) = {
    println("m(" + i + ")")
    i + 1
  }

  evt beforeM[Int] = beforeExec(m)
  evt afterM[Int, Int] = afterExec(m)

  beforeM += reactBefore _
  afterM += reactAfter _

  def reactBefore(i: Int) = println("before m(" + i + ")")
  def reactAfter(i: Int, res: Int) = println("after m(" + i + ") = " + res)

}

object Test {
  def main(args: Array[String]) {
    val c = new C
    c.m(4)
  }
}

// vim: set ts=4 sw=4 et:
