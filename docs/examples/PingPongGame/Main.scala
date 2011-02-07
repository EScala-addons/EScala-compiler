package scala.events.pingpong
import scala.swing.event._

import java.awt.event.ActionListener

import javax.swing.Timer

import scala.swing._

object Main extends SimpleSwingApplication {

  Ball.balls += {
    val b = new Ball(10);
    b.position = (50, 50);
    b.velocity = ((Math.random * 5).intValue(), (Math.random * 5).intValue());
    b
  }

  Mover;

  def top = new MainFrame {
    val timer = new Timer(50, new ActionListener {
      override def actionPerformed(ev: java.awt.event.ActionEvent): Unit = { Clock.clk() }
    }); timer.start(); Clock.clk += (_ => repaint)
    title = "Ping Pong"
    contents = new Component {
      preferredSize = new Dimension(500, 500)
      override def paintComponent(g: Graphics2D) = {
        super.paintComponent(g)
        //paint

        modelDraw(UpperWall, g)
        modelDraw(LowerWall, g)
        modelDraw(Bar1, g)
        modelDraw(Bar2, g)
        modelDraw(Goal1, g)
        modelDraw(Goal2, g)
        Ball.balls.foreach(b => modelDraw(b, g))
      };

      listenTo(keys); println("raections reg.")

      reactions += {
        case KeyPressed(src, key, mod, value) => {
          println("KeyPressed")
          key match {
            case Key.Up => Player1.upKeydown()
            case Key.Down => Player1.downKeydown()
            case Key.W => Player2.upKeydown()
            case Key.S => Player2.downKeydown()
          }
        }
        case KeyReleased(src, key, mod, value) => {
          key match {
            case Key.Up => Player1.upKeyup()
            case Key.Down => Player1.downKeyup()
            case Key.W => Player2.upKeyup()
            case Key.S => Player2.downKeyup()
          }
        }
        case _ => println("Foo")

      }
      this.requestFocus()
    //  publish(KeyPressed(null,Key.Up,0,Key.Location))
    }

  }

  def modelDraw(o: ModelObject, g: Graphics2D) = {
    o match {
      case Ball(r) => g.fillOval(o.position._1, o.position._2, r, r)
      case _ => g.fillRect(o.position._1, o.position._2, o.boundingBox._1, o.boundingBox._2)
    }
  }

}