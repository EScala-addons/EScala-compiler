package scala.events.pingpong
import java.awt.event.ActionListener

import javax.swing.Timer

import scala.swing._

object Main extends SimpleSwingApplication {
	
	Ball.balls += {
		val b = new Ball(10);
		b.position = (50,50); 
		b.velocity = ((Math.random*30).intValue,10+Math.random.intValue);
		b}
	Clock.clk += (_ => Ball.balls.foreach(b => Mover.move(b)))
	
	def top = new MainFrame {
		val timer = new Timer(50,new ActionListener{
			override def actionPerformed(ev: java.awt.event.ActionEvent):Unit = {Clock.clk()}
		});		timer.start();		Clock.clk += (_ => repaint)
		title = "Ping Pong"
		contents = new Component {
			preferredSize = new Dimension(500,500)
			override def paintComponent(g: Graphics2D) = {
				super.paintComponent(g)
				//paint
			
				modelDraw(UpperWall,g)
				modelDraw(LowerWall,g)
				modelDraw(Bar1,g)
				modelDraw(Bar2,g)
				modelDraw(Goal1,g)
				modelDraw(Goal2,g)
				Ball.balls.foreach(b => modelDraw(b,g))
			}
		}
	}
	
	def modelDraw(o : ModelObject, g : Graphics2D) = {
		o match {
			case Ball(r) => g.fillOval(o.position._1,o.position._2,r,r)
			case _ => g.fillRect(o.position._1,o.position._2,o.boundingBox._1,o.boundingBox._2)
		}
	}
	
}