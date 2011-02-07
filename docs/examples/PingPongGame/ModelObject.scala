package scala.events.pingpong

import java.awt.event.KeyEvent
import scala.collection.mutable.ListBuffer
import scala.events._

abstract class ModelObject(pos: (Int, Int) = (0, 0)) {

  var position = pos
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

case class Ball(var radius: Int, pos: (Int, Int)) extends ModelObject(pos) {

  override def isCollidingWith(other: ModelObject) = {
    other match {
      case Ball(r, p) => {
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

case class Bar(var length: Int, pos: (Int, Int)) extends ModelObject(pos) {
  val width = 5;
  velocity = (Math.random.intValue, Math.random.intValue)
  override def boundingBox = (width, length)
}

case class Wall(val length: Int, val pos: (Int, Int)) extends ModelObject(pos) {
  val height = 30;
  override def boundingBox = (length, height)
}

case class Goal(val height: Int, pos: (Int, Int)) extends ModelObject(pos) {
  val width = 30
  override def boundingBox = (width, height)
}

class Player(moveUpKeyCode : Int, moveDownKeyCode : Int, world: World, bar: ModelObject, goal: ModelObject) {
  
  val pressMoveUp = world.keyPressed && ((evt: KeyEvent) => evt.getKeyCode == moveUpKeyCode)  
  val pressMoveDown = world.keyPressed && ((evt: KeyEvent) => evt.getKeyCode == moveDownKeyCode)
  
  var releaseTime : Long = 0;
  
  pressMoveUp += ((_) => releaseTime = System.currentTimeMillis+120)
  pressMoveDown += ((_) => releaseTime = System.currentTimeMillis+120)
  
  val moveBarUp = new BetweenEvent(pressMoveUp, world.clock && ((t : Long) => t > releaseTime));
  val moveBarDown = new BetweenEvent(pressMoveDown, world.clock && ((t : Long) => t > releaseTime));
  
  moveBarUp.before += ((_) => {
	  bar.velocity = (0,-5)
  })
  moveBarUp.after += ((_) => {
	  bar.velocity = (0,0)
  })
  
  moveBarDown.before += ((_)=>bar.velocity =(0,5))
  moveBarDown.after += ((_)=> bar.velocity  = (0,0))
  
}

class World(val size: (Int, Int)) {

  /// Events
  val keyPressed = new ImperativeEvent[KeyEvent]
  val keyReleased = new ImperativeEvent[KeyEvent]
  val clock = new ImperativeEvent[Long]
  
  /// Values
  val upperWall = new Wall(length = size._1, pos = (0, 0));
  val lowerWall = new Wall(length = size._1, pos = (0, size._2 - 30));

  val player1Bar = new Bar(length = size._2 / 4, pos = (50, size._2 / 2 - size._2 / 8));
  val player2Bar = new Bar(length = size._2 / 4, pos = (size._1 - 50, size._2 / 2 - size._2 / 8));

  val player1Goal = new Goal(size._2, (0, 0))
  val player2Goal = new Goal(size._2, (size._1 - 30, 0))

  val player1 = new Player(87, 83, this, player1Bar, player1Goal)
  val player2 = new Player(38, 40, this, player2Bar, player2Goal)

  val objects = List(player1Bar,
    player2Bar,
    upperWall,
    lowerWall,
    player1Goal,
    player2Goal,
    new Ball(radius = 10, pos = (size._1 / 2, size._2 / 2)))
  
  val mover = new Mover(this)  
  clock += (_ => this.objects.foreach(b => mover.move(b)))
}