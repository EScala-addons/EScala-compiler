class C1 {
  evt e[Unit] = emptyevent
}
class C2 extends C1 {
  override evt e[Int] = emptyevent
}

// vim: set ts=4 sw=4 et:
