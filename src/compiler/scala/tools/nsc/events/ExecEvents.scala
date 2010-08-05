package scala.tools.nsc
package events

import transform._
import symtab._
import Flags._
import util.EventUtil
import typechecker._

/**
 * Transforms the implicit event references to an acces to the generated event.
 *
 * @author Lucas Satabin
 */
abstract class ExecEvents extends Transform with TypingTransformers with EventUtil {
  import global._
  import definitions._

  val phaseName: String = "execevents"

  def newTransformer(unit: CompilationUnit): Transformer = {
    new ExecEventsTransformation(unit)
  }

  /** Create a new phase which applies transformer */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      newTransformer(unit) transformUnit unit
    }
  }

  class ExecEventsTransformation(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = {
      tree match {
        case ee @ ExecEvent(kind, meth) =>
          val methSymbol = meth.symbol
          methSymbol.tpe match {
            case mt @ MethodType(params, retType) =>
              // build the string representing the parameters
              val eventName = kind match {
                case BeforeExec() => buildBeforeEventName(methSymbol)
                case AfterExec() => buildAfterEventName(methSymbol)
                case Execution() => buildExecutionEventName(methSymbol)
              }

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
        case _ => super.transform(tree)
      }
    }
  }

}

// vim: set ts=4 sw=4 et:
