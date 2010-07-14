class C1 {
  // not instrumented
  def m(i: Int) = {
    println("m(" + i + ")")
    i + 1
  }
}
class C2 extends C1 {
  // m is instrumented here
  evt eB[Int] = beforeExec(this.m)
  evt eA[Int, Int] = afterExec(m)

  eB += reactB _
  eA += reactA _

  def reactB(i: Int) {
    println("before m(" + i + ")")
  }

  def reactA(i: Int, res: Int) {
    println("after m(" + i + "): res = " + res)
  }
}
object Test {
  def main(args: Array[String]) {
    val c = new C2
    c.eB += reactB _
    c.eA += reactA _
    c.m(4)
  }
  def reactB(i: Int) {
    println("test before m(" + i + ")")
  }

  def reactA(i: Int, res: Int) {
    println("test after m(" + i + "): res = " + res)
  }
}

// vim: set ts=4 sw=4 et:
