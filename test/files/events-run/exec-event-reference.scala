class C {
  observable def m(i: Int) = {
    println("m(" + i + ")")
    i + 1
  }
}

object Test {
  def main(args: Array[String]) {
    val c = new C
    beforeExec(c.m) += reactBefore _
    afterExec(c.m) += reactAfter _
    c m 4
  }

  def reactBefore(i: Int) = println("react before m(" + i + ")")

  def reactAfter(i: Int, res: Int) = println("react after m(" + i + ") result: " + res)

}

// vim: set ts=4 sw=4 et:
