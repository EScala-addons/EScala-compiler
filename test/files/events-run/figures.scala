import scala.events._
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

abstract class IFigure {
    evt resized[Unit] 
    evt moved[Unit]   
    evt geomChanged[Unit]
    evt changed[Unit]    
    evt invalidated[Rectangle] 
    
    def getBounds() : Rectangle
    def getColor() : Int
    
    def moveBy(dx : Int, dy : Int)  
    def setColor(c : Int)  
}

abstract class Figure extends IFigure {    
    protected[this] var color = 0
    
    evt moved[Unit] = afterExec(moveBy)
    evt geomChanged[Unit] = resized || moved  
    evt changed[Unit] = geomChanged || afterExec(setColor)    
    evt invalidated = changed.map((_:Any) => getBounds)  
    
    def getColor() = color  
    
    def setColor(c : Int) { color = c }
}

class RectangleFigure (var rect : Rectangle) extends Figure {
    override evt resized[Unit] = afterExec(resize) || afterExec(setBounds)
    override evt moved[Unit] = super.moved || afterExec(setBounds)
    
    def getBounds() : Rectangle = rect 
    
    def moveBy(dx : Int, dy : Int) {
      rect.origin.moveBy(dx, dy)  
    }
    
    def resize(sx : Int, sy : Int) { 
      rect.setSize(sx, sy) 
    }
    
    def setBounds(bounds : Rectangle) { 
      rect = bounds 
    }   
}

class PolylineFigure(from: Point, to : Point) extends Figure {
    protected[this] var points = new ListBuffer[Point]   
    points += from
    points += to

    def this() = this(new Point(0,0), new Point(0,0))    
    
    protected[this] evt pointsChanged[Unit] = 
      afterExec(changePoint) || afterExec(insertPoint)
      
    override evt resized[Unit] = pointsChanged
    override evt moved[Unit] = super.moved || pointsChanged
    
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
    }
    
    def changeStart(pt : Point) {
      changePoint(0, pt)  
    }
    
    def changeEnd(pt : Point) {
      changePoint(points.length - 1, pt)  
    }
    
    def changePoint(index : Int, pt : Point) { 
      points(index) = pt
    }
    
    def insertPoint(index : Int, pt : Point) { 
      points.insert(index, pt)
    }
}

class Connector(val start: Figure, 
                val end: Figure, 
                protected[this] val connFigure: PolylineFigure) {
                    
    updateStart()
    updateEnd()
    
    start.geomChanged += updateStart _
    end.geomChanged += updateEnd _
        
    observable def updateStart() {
        connFigure.changeStart(start.getBounds().center()) 
    }
    
    observable def updateEnd() {
        connFigure.changeEnd(end.getBounds().center())
    }
}

class MutableDrawing {
    val figures = new VarList[Figure]
    evt invalidated = figures.any(_.invalidated)
    
    def addFigure(fig: Figure) { figures += fig }
    def removeFigure(fig: Figure) { figures -= fig }
}

class DrawingView(val drawing: MutableDrawing) {
    drawing.invalidated += repaint _
    
    def repaint(rect : Rectangle) {
      println("Drawing repainted")
    }
    
    def dispose {
      drawing.invalidated -= repaint _
    }    
}

object Test {
  def main(args: Array[String]) {
      
      // setup the drawing
      val drawing = new MutableDrawing
      val view = new DrawingView(drawing)
      
      val rect1 = new RectangleFigure(new Rectangle(0, 0, 5, 5))
      val rect2 = new RectangleFigure(new Rectangle(50, 50, 5, 5))
      var line = new PolylineFigure()
      drawing.addFigure(rect1)
      drawing.addFigure(rect2)
      drawing.addFigure(line)
      
      val conn = new Connector(rect1, rect2, line)
      
      // trace the events of interest
      afterExec(conn.updateStart) += connUpdated _ 
      afterExec(conn.updateEnd) += connUpdated _
      line.changed += lineChanged _
      rect1.changed += rectChanged _
      rect2.changed += rectChanged _
       
      // change the drawing
      rect1.moveBy(2, 2)
      rect1.setColor(5)
      rect1.resize(6, 6)
      rect1.setBounds(new Rectangle(10, 10, 5, 5))
      
      rect2.moveBy(2, 2)
      rect2.setColor(5)
      rect2.resize(7, 7)
      
      // unregister all reactions
      afterExec(conn.updateStart) -= connUpdated _ 
      afterExec(conn.updateEnd) -= connUpdated _
      line.changed -= lineChanged _
      rect1.changed -= rectChanged _
      rect2.changed -= rectChanged _      
      view.dispose      
      println("The remaining changes are not observed")
      
      // change the drawing again
      rect1.setColor(5)
      rect1.moveBy(2, 2)
  }
  
  def connUpdated(x: (Unit, Unit)) {
      println("Connector updated")
  }
  
  def rectChanged() {
      println("Rectangle changed")
  }
  
  def lineChanged() {
      println("Line changed")
  }
}
