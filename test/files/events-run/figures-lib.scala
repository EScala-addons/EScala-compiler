import scala.events._
import EventsLibConversions._

/**
 * @author Lucas Satabin
 * @author Vaidas
 */

/*
 * The base class of various figures
 */
class Figure(val id: Int) {
  
  /*
   * Instance variables
   */  
  private var color: Int = 0
  private var location: (Int,Int) = (0,0)
  
  /*
   * Operations 
   * 
   * (declared as lazy vals to enable binding and unbinding)
   * (lifted for observation of before and after events) 
   */
  
  // example of an implicitly lifted method  
  lazy val setColor: Observable[Int, Unit] = (c: Int) => 
	  this.color = c
  
  
  // example of an explicitly lifted method
  lazy val setLocation = Observable((pt : (Int, Int)) => 
	  location = pt
  )
  
  /*
   * Events 
   */
  
  // the event is triggered after setColor, if the color is different from 
  // the color of the figure before executing the setColor
  lazy val colorChanged = (setColor.before.map((_: Int) => color)
		                   then 
		                   setColor.after.map((col: Int, _:Unit) => col)
		                   ) && ((oldCol: Int,newCol: Int) => oldCol != newCol)
  
  // figure changed event
  lazy val figureChanged: Event[Unit] = colorChanged || setLocation.after

  /*
   * Reactions
   */
  
  lazy val onFigureChanged = (_ : Unit) => {
	println("Figure " + id + " changed")
  }
  
  /*
   * Event reaction bindings 
   */  
  figureChanged += onFigureChanged  
}

/*
 * Concrete figure classes (to be expanded)
 */
class Circle(id: Int) extends Figure(id)

class Rectangle(id: Int) extends Figure(id)


/*
 * A base class for handles manipulating a figure
 */
class FigureHandle(val fig: Variable[Figure]) {
  
  /*
   * Events	
   */
	
  // reference to the event of the owner figure
  lazy val figureChanged = fig.event(f => f.figureChanged)
  
  /*
   * Reactions
   */
  lazy val onFigureChanged = (_ : Unit) => {
     println("Figure " + fig.value.id + " changed in handle")
  }
  
  /*
   * Event reaction bindings 
   */
  figureChanged += onFigureChanged
}

/*
 * A drawing containing multiple figures
 */
class Drawing {
	
  /*
   * Instance variables
   */

  // the (observable) list of figures 	
  lazy val figures = new VarList[Figure]

  /*
   * Operations
   */  
  
  def contains(f: Figure) = figures.exists(fig => f == fig)

  def +=(fig : Figure) = { figures += fig }
  def -=(fig : Figure) = { figures -= fig }
  
  /*
   * Events
   */
  
  // drawing change = change of one of the figures
  lazy val listChanged = figures.elementAdded || figures.elementRemoved
  lazy val drawingChanged = figures.any((f : Figure)=> f.figureChanged) || listChanged
  
  // an explicit event to force refreshing
  lazy val refresh = new ImperativeEvent[Unit]

  // refresh the drawing if refresh is triggered after one or more drawing changes
  // the event is defined recursively
  lazy val refreshNeeded: Event[Unit] = drawingChanged then (refresh || (drawingChanged then refreshNeeded))
  
  /*
   * Reactions
   */
  
  lazy val onDrawingChanged = (_ : Unit) => {
     println("Drawing changed")
  }
  
  lazy val onRefresh = (_: Unit) => {
	  println("Drawing refreshed")
  }

  /*
   * Event-reaction bindings 
   */
  
  refreshNeeded += onRefresh
  drawingChanged += onDrawingChanged

  def deactivate = {
	  refreshNeeded -= onRefresh
      drawingChanged -= onDrawingChanged
  }  
}

object Test {

  def main(args : Array[String]) {
		
    val fig1 = new Figure(1)
    val fig2 = new Figure(2)
    
    fig1.setColor(5) // figure changed
    fig1.setColor(5) // figure not changed    
    
    val handle = new FigureHandle(fig1);
    val drawing = new Drawing;
    
    drawing.refresh() // refresh not needed
    
    drawing += fig1; // drawing changed
    drawing += fig2; // drawing changed     

    fig1.setLocation(5,4) // drawing and handle affected
    fig2.setColor(3)      // drawing affected  
    
    drawing.refresh() // refresh needed    
    drawing.refresh() // refresh not needed
    
    handle.fig.value = fig2; // change the figure in handle
    drawing -= fig2          // remove one figure from the drawing
    
    fig1.setLocation(5,4) // drawing affected 
    fig2.setColor(4)      // handle affected
    
    drawing.refresh() // refresh needed        
  }
}

