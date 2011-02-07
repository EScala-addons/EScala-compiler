package scala.events.pingpong

import scala.swing.event._
import java.awt.event.ActionListener
import javax.swing.Timer
import scala.swing._;
import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;

object Main extends SimpleSwingApplication {

  var world = new World(500, 300);

  def top = new MainFrame {

    val resetAction = Action("reset") { reset };
    val quitAction = Action("quit") { System.exit(0) };

    menuBar = new MenuBar {
      contents += new Menu("Game") {
        contents += new MenuItem(resetAction);
        contents += new MenuItem(quitAction)
      }
    };

    this.peer.addKeyListener(new KeyListener() {
      def keyPressed(e: KeyEvent) {

        //Inform the world about the key down event
        world.keyPressed(e)
      };

      def keyReleased(e: KeyEvent) {
        //Inform the world about the key-up event
        world.keyReleased(e)
      };

      def keyTyped(e: KeyEvent) {
        //Here you can access the keyTyped-Event
      }
    });

    val timer = new Timer(50, new ActionListener {
      override def actionPerformed(ev: java.awt.event.ActionEvent): Unit = { world.clock(System.currentTimeMillis) }
    });

    timer.start();
    world.clock += (_ => repaint)

    title = "Ping Pong"
    contents = new Component {
      preferredSize = new Dimension(world.size._1, world.size._2)
      override def paintComponent(g: Graphics2D) = {
        super.paintComponent(g)
        world.objects.foreach(drawable => modelDraw(drawable, g));
      };

      this.requestFocus()
    }
  }

  def reset() = {
    println("reset")
    world = new World(top.size.width, top.size.height);
  }

  def modelDraw(o: ModelObject, g: Graphics2D) = {
    o match {
      case Ball(r, position) => g.fillOval(o.position._1, o.position._2, r, r)
      case _ => g.fillRect(o.position._1, o.position._2, o.boundingBox._1, o.boundingBox._2)
    }
  }
}