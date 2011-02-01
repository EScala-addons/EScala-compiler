package scala.tools.nsc
package events

import transform._
import symtab._
import Flags._
import util.EventUtil
import typechecker._

/**
 * This class allows to intrument the observable class.
 *
 */
abstract class AllInstances extends Transform 
                                         with EventUtil
                                         with TypingTransformers
                                         with AllInstancesUtil
                                         {
  import global._
  import definitions._

  val phaseName: String = "allinstances"

  protected var namer: analyzer.Namer = null

  def newTransformer(unit: CompilationUnit): Transformer = {
    new AllInstancesTrans(unit)
  }

  /** Create a new phase which applies transformer */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      namer = analyzer.newNamer(analyzer.rootContext(unit))
      newTransformer(unit) transformUnit unit
    }
  }

  class AllInstancesTrans(unit: CompilationUnit) extends TypingTransformer(unit) { 

    import symtab.Flags._

    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        // Matches "allInstances[generic].event"
        case sel @ Select(TypeApply(allInstances, (generic: Tree) :: Nil), event)
        // Matches "allInstances[generic](_.event)"
        /*case sel @ Apply(TypeApply(allInstances, (generic: Tree) :: Nil),
         *                (event: Tree) :: Nil
         *          )
         */
          if (allInstances.symbol == MethAllInstances) =>
            if (settings.Yeventsdebug.value)
              println("Encountered the allInstances symbol. Parameter: "+generic)
            /*
             * TODOs:
             *   - 'generic' contains the name of the package (e.g.
             *     truie.Transaction) and that trigger an error "symbol no
             *     found". Why ?
             *   - generic$all.all.any(_ => _.event) returns an
             *     "EventNodeExists" but the ValDef that takes this value
             *     has been typed beforehand and doesn't have this type. (in
             *     the case of "evt blah =
             *     beforeEvent(allInstances[C].event)", the type is
             *     ImperativeEvent for instance)
             *      - Attempts to use another syntax (allInstances[C](event))
             *     do not work: "event" doesn't refer to a member of an
             *     instance of C, here ("symbol event no found").
             *      - Attempts to modify the dummy function "allInstances[C]:
             *      C" to make it return an EventNodeExists did not make it:
             *      EventNodeExists takes parameters... how to get them ?
             *     Maybe we will have to put sth. in the parser, after all ?
             */
            // generic$all.all.any
            val allMemberAny = Select(
                  Select(
                    Ident(generic+"$all"),
                    //Ident(newTermName("Transaction$all")),
                    newTermName("all")
                  ),
                  newTermName("any")
            )
            println("allMemberAny = " + allMemberAny)

            // _ => _.event
            val mapEvent = Function(
              List(ValDef(NoMods, "_", generic, EmptyTree)),
              Select(
                //Ident(newTermName("_")), event.symbol
                Ident("_"), event
              )
            )
            println("mapEvent = " + mapEvent)

            // generic$all.all.any(((_: C) => _.event))
            val anyApply = Apply(allMemberAny, List(mapEvent))
            println("anyApply = " + anyApply)

            localTyper.typed(
              atPos(sel.pos) {anyApply}
            )

        case _ => super.transform(tree)
      }
    }

  }
}
