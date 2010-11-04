import scala.events._
class C {
  evt e[Unit] = emptyevent
}
class C2 extends C {
  evt e[Unit] = emptyevent
}

// vim: set ts=4 sw=4 et:
