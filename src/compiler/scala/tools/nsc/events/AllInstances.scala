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
        // Matches "allInstances[generic]"
        case ta @ TypeApply(allInstances, (generic: Tree) :: Nil)
        if (allInstances.symbol == MethAllInstances) =>
          if (settings.Yeventsdebug.value)
            println("Encountered the allinstances symbol. Parameter: "+generic)
          val oldNamer = namer
          namer = analyzer.newNamer(namer.context.make(tree, sym, sym.info.decls))

          // generic$all
          val objname = generic.symbol.rawname+"$all"
          val objsymbol = generic.symbol.owner.info.decl(objname)
          if (objsymbol == NoSymbol) {
            unit.error(tree.pos, objname + " not found. Make sure " +
                       generic.symbol.rawname + " is observable.")
            return EmptyTree
          }
          val objall = Ident(objsymbol)
          
          // generic$all.all
          val allMember = Select(objall, newTermName("all"))
          
          namer.enterSyntheticSym(allMember)
          namer = oldNamer
          localTyper.typed(atPos(ta.pos)(allMember))

        // Matches "anyInstance[generic].event"
        case sel @ Select(TypeApply(anyInstance, (generic: Tree) :: Nil), event) =>

          if (anyInstance.symbol == MethAnyInstance) {
            if (settings.Yeventsdebug.value)
              println("Encountered the anyInstance symbol. Parameter: "+generic)
            val oldNamer = namer
            namer = analyzer.newNamer(namer.context.make(tree, sym, sym.info.decls))

            // generic$all
            val objname = generic.symbol.rawname+"$all"
            val objsymbol = generic.symbol.owner.info.decl(objname)
            if (objsymbol == NoSymbol) {
              unit.error(tree.pos, objname + " not found. Make sure " +
                         generic.symbol.rawname + " is observable.")
              return EmptyTree
            }
            val objall = Ident(objsymbol)

            // generic$all.all.any
            val allMemberAny = Select(
                    Select(objall,
                    newTermName("all")
                  ),
                  newTermName("any")
            )

            // _ => _.event
            val mapEvent = Function(
              List(ValDef(NoMods, "_", generic, EmptyTree)),
              Select(
                Ident("_"), event
              )
            )

            // generic$all.all.any(((_: C) => _.event))
            val anyApply = Apply(allMemberAny, List(mapEvent))
            namer.enterSyntheticSym(anyApply)
            namer = oldNamer
            localTyper.typed(atPos(sel.pos)(anyApply))

        } else {
            super.transform(tree)
        }

        case _ => super.transform(tree)
      }
    }

  }
}
