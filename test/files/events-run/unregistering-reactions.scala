object Test {
  def main(args: Array[String]) {
    val c = new C
    c.e((1,2))
    c.e -= c.react2 _
    c.e((3,4))
  }
}

class C {
  imperative evt e[(Int,Int)]

  e += react1 _
  e += react2 _

  def react1(i1: Int, i2: Int) {
    println("react1 (" + i1 + ", " + i2 + ")")
  }
  def react2(i1: Int, i2: Int) {
    println("react2 (" + i1 + ", " + i2 + ")")
  }
}

// vim: set ts=4 sw=4 et:
