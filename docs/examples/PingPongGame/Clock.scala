package scala.events.pingpong
import scala.events.ImperativeEvent

object Clock {

  val clk = new ImperativeEvent[Unit]

}

object Mover {

  Clock.clk += (_ => { move(Bar1); move(Bar2) })
  val moved = new ImperativeEvent[ModelObject]

  def move(o: ModelObject) {
    if (o.velocity == (0, 0)) return
    o.position = (o.position._1 + o.velocity._1, o.position._2 + o.velocity._2)
    moved(o)
  }

  val reverseYVelocity = ((o: ModelObject) => {
    o.velocity = (o.velocity._1, -o.velocity._2)
  })
  val reverseXVelocity = ((o: ModelObject) => {
    o.velocity = (-o.velocity._1, o.velocity._2)
  })

  moved && (o => colliding(o, UpperWall)) += reverseYVelocity
  moved && (o => colliding(o, LowerWall)) += reverseYVelocity

  val ballMoved = moved && (o => o.isInstanceOf[Ball]) map ((o: ModelObject) => o.asInstanceOf[Ball])

  ballMoved && (o => colliding(o, Bar1) || colliding(o, Bar2)) += reverseXVelocity
  ballMoved && (o => colliding(o,Goal1)) += (_ => "Point for Player2")
  ballMoved && (o => colliding(o,Goal2)) += (_ => "Point for Player1")

  def colliding(o1: ModelObject, o2: ModelObject) = o1.isCollidingWith(o2) || o2.isCollidingWith(o1)

}