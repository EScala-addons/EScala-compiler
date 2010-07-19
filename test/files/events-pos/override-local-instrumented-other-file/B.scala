class B {
  def m() = println("m in B")
  evt e[Unit] = beforeExec(m)
}

// vim: set ts=2 sw=2 et:
