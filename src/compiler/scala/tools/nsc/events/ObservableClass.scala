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

    private var obsobjects: List[Tree] = null
    
    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
          case pd: PackageDef =>
                val oldNamer = namer
                namer = analyzer.newNamer(namer.context.make(tree, sym, sym.info.decls))
                var oldobsobjects = obsobjects
                obsobjects = List()
                val tpack = super.transform(pd).asInstanceOf[PackageDef]                 

                //add the observable classes object
                //and generate result
                val result = treeCopy.PackageDef(tpack, tpack.pid, 
                        obsobjects ::: tpack.stats)

                namer = oldNamer

                obsobjects = oldobsobjects
                result
          case cd @ ClassDef(mods, name, tparams, impl) =>
            // transform the class body // TODO ???
            val oldNamer = namer
            namer = analyzer.newNamer(namer.context.make(tree, sym, sym.info.decls))

            if (sym.isInstrumented) {
              if (settings.Yeventsdebug.value) {
                  println("Transform of observable class called for :  " + name)
              }
              val pos = sym.pos
              val parents = List(genAllObjectTpt(TypeTree(sym.tpe)), TypeTree(ScalaObjectClass.tpe))
              val newobj = atPos(pos)(ModuleDef ( NoMods, 
                                      name+"$all", 
                                      Template(parents,emptyValDef, NoMods, List(Nil), List(Nil), Nil, pos)
                                      ))
              oldNamer.enterSyntheticSym(newobj)
              obsobjects = localTyper.typed(newobj).asInstanceOf[ModuleDef] :: obsobjects
              //obsobjects = newobj:: obsobjects
            }
            namer = oldNamer
             
            tree 
          case _ => super.transform(tree)
        }
    }
 }

}
// vim: set ts=4 sw=4 et:
