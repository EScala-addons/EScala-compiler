class C1 {
  def m(i: Int) = ()
  // legal
  evt e[Int] = beforeExec(m)
}
class C2(c1: C1) {
  // illegal
  evt e[Int] = beforeExec(c1.m)
}

// vim: set ts=4 sw=4 et:
