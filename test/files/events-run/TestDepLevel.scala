package micro
import scala.events._
import behaviour._
import java.text.SimpleDateFormat
import java.util.Date
/**
 * Testing Scenario
 * MultiDependency Handling:
 * Behaviours referenced in Behaviours
 * --> correct dep.level calculation
 * --> correct dep reeval on Var change
 * 
 */

object TesterDepLevel {
  protected val tstampFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SS ")
  def tstamp = tstampFormat.format(new Date)

  def main(args: Array[String]) = {
    Console.err.println("STARTING TESTING")
    
    println("logging start: " + tstamp)
    
    val x = new Var(true)
    val a = new Var(1)
    val b = new Var(2)

    // beh1.level = 1 (max(0+1, -1)), val: 6
    val beh1 = Signal { a() + 5 }

// beh2.level = 1 (max(0+1, -1)) 
//    val beh2 = Signal { b() + 10 }
    // beh3.level = 2 (max(1+1, 0+1, -1)), val: 7
    val beh3 = Signal { beh1() + 10 }
    

// beh4.level = 1 (max(0+1, -1))
//    val beh4 = Signal { b() + 2 }

    // beh5.level = 3 (max(2+1, 0+1, -1)), val: 8
    val beh5 = Signal { beh3() + 20 }
    

    // beh6.level = 4 (max(1+1, 3+1, 2+1, -1)), val: 21
    val beh6 = Signal { beh1() + beh5() + beh3() }
//    val beh6 = Signal { 12 + b() }
    
    
    
    println("##################")
    println("Var a myDeps: " + a.myDeps.toList)
    println("##################")
    beh1.changed += beh1Changed _
    beh3.changed += beh3Changed _
    beh5.changed += beh5Changed _
    beh6.changed += beh6Changed _
    
    println("beh1.level: " + beh1.getLevel())
//    println("beh1 REPRESENTING DEP: " + beh1.getDep())
//    println("beh1.myDeps: " + beh1.getMyDeps() + " ---- size: " + beh1.getMyDeps().size)
//    println("_____________---------------______________")
    println("beh3.level: " + beh3.getLevel())
//    println("beh3 REPRESENTING DEP: " + beh3.getDep())
//    println("beh3.myDeps: " + beh3.getMyDeps() + " ---- size: " + beh3.getMyDeps().size)
//    println("_____________---------------______________")
    println("beh5.level: " + beh5.getLevel())
//    println("beh5 REPRESENTING DEP: " + beh5.getDep())
//    println("beh5.myDeps: " + beh5.getMyDeps() + " ---- size: " + beh5.getMyDeps().size)
//    println("_____________---------------______________")
    println("beh6.level: " + beh6.getLevel())
//    println("beh6 REPRESENTING DEP: " + beh6.getDep())
//    println("beh6.myDeps: " + beh6.getMyDeps() + " ---- size: " + beh6.getMyDeps().size)
//    println("_____________---------------______________")
    // ordered updateList for a.update: beh1, beh3, beh5, beh6
    a() = 10
    
//    a() = 5
//
//    a() = 2
    
    def beh1Changed(newVal : Int) = {
      println("beh1 changed: (old, new): " + newVal)
//      println("Testing beh6 (dependent but eval later: " + beh6() + ")")
    }
    
    def beh3Changed(newVal : Int) = {
      println("beh3 changed: (old, new): " + newVal)
    }
    
    def beh5Changed(newVal : Int) = {
      println("beh5 changed: (old, new): " + newVal)
    }
    
    def beh6Changed(newVal : Int) = {
      println("beh6 changed: (old, new): " + newVal)
    }
    
    println("logging start: " + tstamp)

    Console.err.println("FINISHED TESTING")
  }

}