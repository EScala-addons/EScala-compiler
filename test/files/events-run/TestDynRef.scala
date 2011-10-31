package micro

import scala.events._
import behaviour._
import java.text.SimpleDateFormat
import java.util.Date

/**
 * Testing Scenario
 *  Dynamic Object-Reference Handling:
 *  here: using event-attribute of changing Figure-instance in behaviour
 *  --> correct reassigning of reactions on instance change   
 */


object TesterDynRef {
  protected val tstampFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SS ")
  def tstamp = tstampFormat.format(new Date)

  def main(args: Array[String]) = {
    
    val start = new Var(new Figure)
    val end = new Var(new Figure)
    
    val con = new Connector(start, end)
    
//    Console.err.println("_____________start defaultId: " + start().getId())
    start().setId(10)
//    Console.err.println("_____________start defined Id: " + start().getId())
    
    
    Console.err.println("_____________moving figure Start of Connector")
    start().move()
    
//    end().move()
    
    val copyOld = start()
//    
    Console.err.println("------------> changing Figure instance in Var-start")
    start() = new Figure
//    Console.err.println("____________start defaultId: " + start().getId())
    start().setId(100)
//    Console.err.println("____________start (new Instance) defaultId: " + start().getId())
    
    Console.err.println("_____________moving new instance Start of Connector")
    start().move()
    
    Console.err.println("_____________moving old INSTANCE of Connector -> no reaction expected!!")
    copyOld.move()
    
    
  }
}

class Figure{
  private var _id = 0
  
  def setId(newId : Int) = { _id = newId }
  def getId() = _id
  
  def move() {
    changed()
  }
  
  def resize() {
    changed()
  }
  //changed = afterExec move || afterExec resize
  lazy val changed = new ImperativeEvent[Unit] 
}

class Connector(start : Var[Figure], end : Var[Figure]) {
  
//  EventSignal{start().changed} += startChanged _
//  Signal{end().changed}.flatten() += endChanged _
  Signal{start().changed}.flatten += startChanged _
  Signal{end().changed}.flatten += endChanged _
  
//  val chStart = Signal{start().changed}
//  val chEnd = Signal{start().changed}  
//  chStart() += startChanged _ 
//  chEnd() += endChanged _
  
  
  def endChanged() = {
    Console.err.println("end changed in connector")
  }
  
  def startChanged() = {
    Console.err.println("start changed in connector with Id: " + start().getId())
  }
}