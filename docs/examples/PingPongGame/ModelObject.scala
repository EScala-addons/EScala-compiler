package scala.events.pingpong
import scala.collection.mutable.ListBuffer

abstract class ModelObject {

  var position: (Int, Int) = (0, 0)
  var velocity: (Int, Int) = (0, 0)
  /**
   * first value is width, second height
   */
  def boundingBox: (Int, Int)

  def isCollidingWith(other: ModelObject): Boolean = {
    val extended_pos = (Math.min(position._1, position._1 + velocity._1),
      Math.min(position._2, position._2 + velocity._2))
    val greaterBB = (Math.max(boundingBox._1, boundingBox._1 + velocity._1),
      Math.max(boundingBox._2, boundingBox._2 + velocity._2))
    val rect = (extended_pos._1, extended_pos._2, greaterBB._1, greaterBB._2);
    insideRect(other.position, rect) ||
      insideRect((other.position._1, other.position._2 + other.boundingBox._2),
        rect) ||
      insideRect((other.position._1 + other.boundingBox._1, other.position._2),
        rect) ||
      insideRect((other.position._1 + other.boundingBox._1, other.position._2 + other.boundingBox._2), rect)
  }

  private def insideRect(point: (Int, Int), Rect: (Int, Int, Int, Int)) = {
    (point._1 > Rect._1) &&
      (point._1 < Rect._1 + Rect._3) &&
      (point._2 > Rect._2) &&
      (point._2 < Rect._2 + Rect._4)
  }

}

case class Ball(var radius: Int) extends ModelObject {
  override def isCollidingWith(other: ModelObject) = {
    other match {
      case Ball(r) => {
        val middle = (other.position._1 + other.boundingBox._1 / 2,
          other.position._2 + other.boundingBox._2 / 2)
        val myMiddle = (position._1 + velocity._1 + boundingBox._1 / 2,
          position._2 + velocity._2 + boundingBox._2 / 2)
        Math.sqrt(Math.pow(middle._1 - myMiddle._1, 2)
          + Math.pow(middle._2 - myMiddle._2, 2)) > r + radius
      }
      case _ => super.isCollidingWith(other)
    }
  }
  override def boundingBox = (2 * radius, 2 * radius)
}

abstract case class Bar(var length: Int) extends ModelObject {
  val width = 5;
  velocity = (Math.random.intValue, Math.random.intValue)
  override def boundingBox = (width, length)
}
abstract case class Wall(val extent: Int) extends ModelObject {
  val height = 30;
  override def boundingBox = (extent, height)
}

abstract case class Goal(val right: Boolean, val height: Int) extends ModelObject {
  val width = 30
  override def boundingBox = (width, height)
}

object Bar1 extends Bar(10) {
  position = (0, 50)
}

object Bar2 extends Bar(10) {
  position = (200, 50)
}

object UpperWall extends Wall(300) {
  position = (-50, 0)
}

object LowerWall extends Wall(300) {
  position = (-50, 100)
}

object Goal1 extends Goal(false, 100 + LowerWall.height) {
  position = (-50, 0)
}

object Goal2 extends Goal(true, 100 + LowerWall.height) {
  position = (250, 0)
}

object Ball {
  val balls = new ListBuffer[Ball]
}