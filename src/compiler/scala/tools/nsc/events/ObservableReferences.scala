package scala.tools.nsc
package events

import transform._
import symtab._
import Flags._
import util.EventUtil
import typechecker._

/**
 * Transforms the references which changed due to the observable transformations. This includes:
 * <ul>
 *  <li>transform implicit event references to an acces to the generated event.</li>
 *  <li>transform super references in implementation method to call the super implementation method.</li>
 * </ul>
 *
 * @author Lucas Satabin
 */
abstract class ObservableReferences extends Transform with TypingTransformers with EventUtil {
  import global._
  import definitions._

  val phaseName: String = "obsrefs"

  def newTransformer(unit: CompilationUnit): Transformer = {
    new ObservableReferencesTransformation(unit)
  }

  /** Create a new phase which applies transformer */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      newTransformer(unit) transformUnit unit
    }
  }

  class ObservableReferencesTransformation(unit: CompilationUnit) extends TypingTransformer(unit) {

    private var meth: DefDef = null

    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      //println("obsREF.................. tree + symbol: " + tree + ",\n " + tree.symbol)

      tree match {
        case dd: DefDef if sym.isOverride && sym.isImplementationMethod =>
          val oldMeth = meth
          println(":::::::::found DefDef, symbol: " + sym)
          meth = dd
          val res = super.transform(tree)
          meth = oldMeth
          res
        case ee @ ExecEvent(kind, meth) =>
          val methSymbol = meth.symbol
          println("------ ee: " + ee)
          println(":::::::::found ExecEvent, symbol: " + methSymbol)
          methSymbol.tpe match {
            case mt @ MethodType(params, retType) =>
              // build the string representing the parameters
              println("execEvent -> looking for Type: " + kind)
              val eventName = kind match {
                case BeforeExec() => buildBeforeEventName(methSymbol)
                case AfterExec() => buildAfterEventName(methSymbol)
                case Execution() => buildExecutionEventName(methSymbol)
              }
              println("execEvent -> resulting eventName: " + eventName)

              // search the symbol in the class
              localTyper.typed(
                atPos(tree.pos) {
                  meth match {
                    case Select(prefix, _) =>
                      Select(prefix, newTermName(eventName)) setSymbol NoSymbol //(prefix.symbol.info.decl(eventName))
                    case _ => /* should not happen here */
                      unit.error(tree.pos, "a reference to a method is expected")
                      EmptyTree
                  }
                }
              )
            case _ => /* should not happen in this phase if the typing phase correctly checks */
              unit.error(tree.pos, "a reference to a method is expected")
              EmptyTree
          }
        case se @ SetEvent(kind, field) =>
           val fieldSymbol = field.symbol
           // PROBLEM: got Fieldreference (!= Methodreference!!) as Tree not a reference to setterMethod
            	println("given RAW se, kind, field: " + se + ",\n " + kind + ",\n " + field + ", fieldsymbol: " + field.symbol)
            	
           val eventName = kind match {
                case BeforeSet() => println("found beforeSet"); buildBeforeEventName(fieldSymbol)
                case AfterSet() => println("found afterSet"); buildAfterEventName(fieldSymbol)
                case _ => println("no catching " + kind)
              }
          	println("resulting eventName: " + eventName)
            super.transform(tree)
              
        case app @ Apply(Select(sup @ Super(qual, mix), n), p) if meth != null => 
          // super call in an observable method
          // call to the super observable method must be replaced 
          // with call to the super implementation method
          
          val pos = sym.pos
            
          // the super called method
          val called = app.symbol
          val calledImplName = buildImplMethodName(called)

          if(calledImplName.decode == meth.symbol.name.decode && called.isInstrumented) {
            // replace super call
            // get the super implementation method
            val superName = buildImplMethodName(sym)
            //val superMeth = called.owner.info.decls.lookup(superName)
              
            atPos(pos)(localTyper.typed(Apply(Select(Super(qual, mix), superName), p)))
          } else 
            super.transform(tree)

        case _ => super.transform(tree)
      }
    }
  }

}

// vim: set ts=4 sw=4 et:
