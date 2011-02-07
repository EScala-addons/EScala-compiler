package scala.events.pingpong
import scala.swing.event._

import java.awt.event.ActionListener

import javax.swing.Timer

import scala.swing._;
import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;

object Main extends SimpleSwingApplication {

	var world = new World(500,300);
	var mover = new Mover(world)

	Clock.clk += (_ => world.objects.foreach(b => mover.move(b)))
	
  def top = new MainFrame {
		
		val resetAction = Action("reset"){reset};
		val quitAction = Action("quit"){System.exit(0)};
		
		menuBar = new MenuBar {
			contents += new Menu("Game") {
				contents += new MenuItem(resetAction);
				contents += new MenuItem(quitAction)
			}
		};
		
		this.peer.addKeyListener(new KeyListener() {
		    def keyPressed(e:KeyEvent) {
		      println(e + " key pressed")
		    };
		
		    def keyReleased(e:KeyEvent) {
		      println(e + " key released")
		    };
		
		def keyTyped(e:KeyEvent) {
		      println(e + " key typed")
		    }
		 });
		
    val timer = new Timer(50, new ActionListener {
      override def actionPerformed(ev: java.awt.event.ActionEvent): Unit = { Clock.clk() }
    }); timer.start(); Clock.clk += (_ => repaint)
    title = "Ping Pong"
    contents = new Component {
      preferredSize = new Dimension(500, 500)
      override def paintComponent(g: Graphics2D) = {
        super.paintComponent(g)
				world.objects .foreach(drawable => modelDraw(drawable, g));
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
	
	def reset() = {
		world = new World(500,300);
		mover = new Mover(world) 
	}
	
  def modelDraw(o: ModelObject, g: Graphics2D) = {
    o match {
			case Ball(r,position) => g.fillOval(o.position._1,o.position._2,r,r)
      case _ => g.fillRect(o.position._1, o.position._2, o.boundingBox._1, o.boundingBox._2)
    }
  }

}