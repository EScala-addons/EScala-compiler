import scala.events._
object Test {

  // built-in events from the mouse
  imperative evt mouseDown[Int, Int]
  imperative evt mouseUp[Int, Int]
  imperative evt mouseMove[Int, Int]

  // combined events
  evt drag[Int, Int] = mouseMove within between(mouseDown, mouseUp)
  evt drop[Int, Int] = (drag.dropParam then mouseUp).map((_: Unit, p: (Int, Int)) => p)

  def main(args: Array[String]) {
    mouseDown += {(x: Int, y: Int) => println("mouse down at (" + x + ", " + y + ")")}
    mouseUp += {(x: Int, y: Int) => println("mouse up at (" + x + ", " + y + ")")}
    mouseMove += {(x: Int, y: Int) => println("mouse move to (" + x + ", " + y + ")")}
    drag += {(x: Int, y: Int) => println("mouse drag to (" + x + ", " + y + ")")}
    drop += {(x: Int, y: Int) => println("mouse drop at (" + x + ", " + y + ")")}

    mouseDown(1, 1)
    mouseUp(1, 1)
    mouseMove(2, 2)
    mouseDown(2, 2)
    mouseMove(2, 3)
    mouseMove(2, 4)
    mouseUp(2, 4)
    mouseMove(2, 5)
  }
}

// vim: set ts=2 sw=2 et:
