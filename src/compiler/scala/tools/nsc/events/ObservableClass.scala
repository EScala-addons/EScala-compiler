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
abstract class ObservableClass extends Transform 
                                         with EventUtil
                                         with TypingTransformers
                                         with ObservableClassUtil
                                         {

  import global._
  import definitions._

  val phaseName: String = "observableclass"

  protected var namer: analyzer.Namer = null

  def newTransformer(unit: CompilationUnit): Transformer = {
    new ObservablesClassTrans(unit)
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

  class ObservablesClassTrans(unit: CompilationUnit) extends TypingTransformer(unit) { 
        
    import symtab.Flags._
    
    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
          case cd @ ClassDef(mods, name, tparams, impl) =>
            // transform the class body // TODO ???
            val oldNamer = namer
            namer = analyzer.newNamer(namer.context.make(tree, sym, sym.info.decls))

            if (sym.isInstrumented) {
              if (settings.Yeventsdebug.value) {
                  println("Transform of observable class called for :  " + name)
              }

              val modifiers = (cd.mods & ~OBSERVABLE & ~DEFERRED & ~INSTRUMENTED) | FINAL

              val allObjectName = name + "$all"
              var allObject = genAllObject(
                  cd,
                  modifiers,
                  allObjectName,
                  genAllObjectTpt(List[Tree](Ident(name))),
                  newAllObject(List[Tree](Ident(name)), name),
                  sym.pos)

              if (settings.Yeventsdebug.value) {
                println("allObject: " + allObject)
              }

              namer.enterSyntheticSym(allObject)
              //allObject = localTyper.typed(allObject).asInstanceOf[ValDef]
              //... TODO

              //tree // is it the proper thing to return ?
            }
            
            tree // is it the proper thing to return ? bis

          case _ => super.transform(tree)
       }
    }

    private def genAllObject(tree: ClassDef, modifiers: Modifiers, name: Name, tpt: Tree, body: Tree, pos: Position) = {
      
      val flags = modifiers | LAZY | (if(settings.Yeventsdebug.value) 0 else SYNTHETIC)
      
      val obj = ValDef(flags, name, tpt, body)
      // TODO must be in the class' parent (?)
      atPos(pos)(obj)
    }
  }
}
// vim: set ts=4 sw=4 et:
