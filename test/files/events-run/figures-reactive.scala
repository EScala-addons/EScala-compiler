import scala.react._
import scala.collection.mutable.ListBuffer

class Point(var x : Int, var y : Int) {
    def moveBy(dx : Int, dy : Int) { 
      x += dx; y += dy 
    }
}

class Size(var x : Int, var y : Int) {    
}

class Rectangle(var origin : Point, var size : Size) {
    def this(x : Int, y : Int, sx : Int, sy : Int) 
      = this(new Point(x, y), new Size(sx, sy))
      
    def setSize(sx : Int, sy : Int) {
        size.x = sx; size.y = sy
    }
    
    def center() : Point = {
        new Point(origin.x + size.x/2, origin.y + size.y/2)
    }
}

abstract class IFigure extends Observing{
	
    val resized : Events[Unit]
    val moved : Events[Unit]
   
    val geomChanged : Events[Unit]
    val changed : Events[Unit]
    val invalidated : Events[Rectangle]
    
    //var observer : Observing
   
    def getBounds() : Rectangle
    def getColor() : Int
    
    def moveBy(dx : Int, dy : Int)  
    def setColor(c : Int)  
}

abstract class Figure extends IFigure {    
    protected[this] var color = 0
    
    // instead of var moved = afterExec(moveBy)
    var afterExecMoveBy = new EventSource[Unit]
    
    // must be lazy or NullPointerException..
    lazy val geomChanged = resized merge moved
    lazy val changed = geomChanged merge afterExecColor
    lazy val invalidated = changed.map((_:Any) => getBounds)
    
    var afterExecColor = new EventSource[Unit]
    
    def getColor() = color  
    
    def setColor(c : Int) { 
    	color = c
    	afterExecColor emit () 
    }
}

class RectangleFigure (var rect : Rectangle) extends Figure {
	  val afterExecResize = new EventSource[Unit]
	  val afterExecSetBounds = new EventSource[Unit]

	  override val resized = afterExecResize merge afterExecSetBounds
    //override var resized = afterExec(resize) || afterExec(setBounds)
    
    // observer = observe(geomChanged) { x => x; true }
    // ERROR: scala super may not be used on var moved(in case: super.moved merge ...)
    override val moved = afterExecMoveBy merge afterExecSetBounds
    //override evt moved[Unit] = super.moved || afterExec(setBounds)
    
    def getBounds() : Rectangle = rect 
    
    override def moveBy(dx : Int, dy : Int) {
      rect.origin.moveBy(dx, dy)  
      afterExecMoveBy emit ()
    }
    
    def resize(sx : Int, sy : Int) { 
      rect.setSize(sx, sy) 
      afterExecResize emit ()
      
    }
    
    def setBounds(bounds : Rectangle) { 
      rect = bounds 
      afterExecSetBounds emit ()
    }   
}

class PolylineFigure(from: Point, to : Point) extends Figure {
    protected[this] var points = new ListBuffer[Point]   
    points += from
    points += to

    def this() = this(new Point(0,0), new Point(0,0))    
        
    //observer = observe(geomChanged) { x => x; true }
    val afterExecChangePoint = new EventSource[Unit]
    val afterExecInsertPoint = new EventSource[Unit]
    protected[this] val pointsChanged = afterExecChangePoint merge afterExecInsertPoint
    //protected[this] evt pointsChanged[Unit] = 
      //afterExec(changePoint) || afterExec(insertPoint)
    
    override val resized = pointsChanged
    // override evt moved[Unit] = super.moved || pointsChanged
    override val moved = afterExecMoveBy merge pointsChanged
    
    def getBounds() : Rectangle = {
      var minX = points(0).x
      var minY = points(0).y
      var maxX = points(0).x
      var maxY = points(0).y      
      for (pt <- points) {
        if (pt.x < minX) minX = pt.x
        if (pt.y < minY) minY = pt.y
        if (pt.x > maxX) maxX = pt.x
        if (pt.y > maxY) maxY = pt.y
      }        
      new Rectangle(minX, minY, maxX - minX, maxY - minY)      
    }
    
    def moveBy(dx : Int, dy : Int) {
      for (pt <- points) {
        pt.moveBy(dx, dy)
      }
      afterExecMoveBy emit ()
    }
    
    def changeStart(pt : Point) {
      changePoint(0, pt)  
    }
    
    def changeEnd(pt : Point) {
      changePoint(points.length - 1, pt)  
    }
    
    def changePoint(index : Int, pt : Point) { 
      points(index) = pt
      afterExecChangePoint emit ()
    }
    
    def insertPoint(index : Int, pt : Point) { 
      points.insert(index, pt)
      afterExecInsertPoint emit ()
    }
}

// Connector reactive: params are signals who are the dependants of inner body of Connector...
// no errors, but in propagation currently no reevaluating...
class Connector (initStart: Figure, initEnd: Figure, 
                protected[this] val connFigure: PolylineFigure) extends Observing{
    
    val start = new Var[Figure](initStart) 
    val end  = new Var[Figure](initEnd) 
      
    var lastStart = start.now
    var lastEnd = end.now
	
		//var obStart = observe(start().geomChanged) { x => updateStart(); true }
		
//		var obEnd = observe(end().geomChanged) { x => updateEnd(); true }
//		println("conn first time observe advice......")

    val startChanged = Signal { start().geomChanged }.flatten merge start.changes 
    observe(startChanged) { x => updateStart(); true }
		
//		Signal{
//			
//			updateStart()
//			updateEnd()
//			
////			if(lastStart != start.now || lastEnd != end.now) 
////			{
////				// remove reaction from lastFigures (if only changes, then other figure (not changed) will loose reaction temporarily ...added again later
////				lastStart.observer.dispose()
////				lastEnd.observer.dispose()
////				println("________________lastFig.sum: " + lastFig().sum) // DEBUG
////			}
//
//
//			// add reaction to current figures
//			observe(start().geomChanged) { x => updateStart(); true }
//			observe(end().geomChanged) { x => updateEnd(); true }
//			
//			// DEBUG
//			println("evaluated........")
//		
//			// update lastFigure (automated via compiler possible??) for rem reaction
//			lastStart = start.now
//			lastEnd = end.now
//	}
        
    def updateStart() {
        connFigure.changeStart(start().getBounds().center())
        println("update start###############") 
    	 	//afterExecUpdateStart emit ()
    }
    
    def updateEnd() {
        connFigure.changeEnd(end().getBounds().center())
        //afterExecUpdateEnd emit ()
        println("update end******************") 
    }
    
//    def dispose()
//    {
//    	obStart.dispose()
//    	obEnd.dispose()
//    }
}

class MutableDrawing extends Observing{
	
	// alternative Implementation of VarList (ListBuffer + custom def invalidated)
    val figures = new Val(new ListBuffer[Figure])
  
  	// defined for simulating VarList with ListBuffer and imperative Events:
  	val elementRemoved = new EventSource[Unit]
  	val elementAdded = new EventSource[Unit]

		var invalidated : Events[Rectangle] = new EventSource[Rectangle]
  	// function for iterating through figure-List to collect invalidated-Event of each figure
    def invalidate() = {
    		for(fig <- figures())
	    		invalidated merge fig.invalidated
	    	invalidated
    	}

    
    def activeInv = Signal { 
    	println("invalidating...")
    	//invalidated.clearDependents()
   		//invalidate()
  	}
  	
  	observe(activeInv) { x => println("reevaluating FigEventList............................"); true }

    def addFigure(fig: Signal[Figure]) { 
    	figures() += fig()
    	//elementAdded emit activeInv()
    }
    def removeFigure(fig: Signal[Figure]) { 
    	figures() -= fig()
    	//elementRemoved emit fig 
    }
}

class DrawingView(val drawing: MutableDrawing) extends Observing {
		
		// lazy defined - else NullPointerException
		lazy val obInvalidated = observe(drawing.invalidated) { x => println("repainting....."); repaint _; true }
    //drawing.invalidated += repaint _
    
    def repaint(rect : Rectangle) {
      println("Drawing repainted with rect (x,y): " + rect.size.x + ", " + rect.size.y)
    }
    
    def dispose {
    	//obInvalidated.dispose()
      //drawing.invalidated -= repaint _
    }    
}

object Test extends Observing {
	// main uncommented for testing in escala-REPL with :load [filename]
  //def main(args: Array[String]) {
      
      // setup the drawing
      //val drawing = new MutableDrawing
      //val view = new DrawingView(drawing)
      
      var rect1 = new RectangleFigure(new Rectangle(0, 0, 10, 10))
      var rect2 = new RectangleFigure(new Rectangle(50, 50, 5, 5))
      var line = new PolylineFigure()
      //drawing.addFigure(rect1)
      //drawing.addFigure(rect2)
//      drawing.addFigure(line)
      
      val conn = new Connector(rect1, rect2, line)
      
      println("moveBy")
      rect1().moveBy(2, 2)
      
      println("set rect2")
      conn.start() = rect2;
//    
      println("moveBy")  
      rect2().moveBy(2, 2)
   
      //val obSig = observe(conn.active()) {x => println("......"); x; true}
      //val obRect1 = observe(rect1.changed) { x => rectChanged("Rect1"); true}
      //val obRect2 = observe(rect2.changed) { x => rectChanged("Rect2"); true}
      //rect1.changed += rectChanged _
      //rect2.changed += rectChanged _
      
      // currently not possible, cause imperative Events not placable in Connector
      // -> NullPointerException...
      // no chance to observe with scala.react.Events imperatively,
      // since constructor of connector would throw exception
      //val obConnStart = observe(conn.afterExecUpdateStart) { x => connUpdated _; true} //afterExec(conn.updateStart) += connUpdated _ 
      //val obConnEnd = observe(conn.afterExecUpdateEnd) { x => connUpdated _; true} //afterExec(conn.updateEnd) += connUpdated _
             
      //rect1().moveBy(2, 2)
//      rect1.setColor(5)
//      rect1.resize(6, 6)
//      rect1.setBounds(new Rectangle(10, 10, 5, 5))
//      
      //rect2().moveBy(2, 2)
//      rect2.setColor(5)
//      rect2.resize(7, 7)
      //rect1().moveBy(2, 2)
      //rect1() = new RectangleFigure(new Rectangle(5, 5, 10, 10))
      //rect1().moveBy(2, 2)
      //rect2() = new RectangleFigure(new Rectangle(12, 12, 10, 10))
      //view.dispose()
      //obRect1.dispose()
      //obRect2.dispose()
      //conn.dispose()
      
      
      
  //}
           
  // (Unit, Unit) 2 type-Params for EventSource in scala.react not possible
  def connUpdated(x: (Unit, Unit)) {
      println("Connector updated")
  }
  
  def rectChanged(changed : String) {
      println(".............................." + changed + " changed")
  }
  
  def lineChanged() {
      println("Line changed")
  }
}

// inserted for testing file in escala-REPL; to remove if testing in events.suite
val tester = Test