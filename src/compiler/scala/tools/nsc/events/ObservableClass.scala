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
                val oldobsobjects = obsobjects
                obsobjects = List()
                val tpack = super.transform(pd).asInstanceOf[PackageDef]                 

                //add the observable classes object
                //and generate result
                val result = treeCopy.PackageDef(tpack, tpack.pid, obsobjects ::: tpack.stats)

                obsobjects = oldobsobjects
                result
          case cd @ ClassDef(mods, name, tparams, impl) =>
            // transform the class body // TODO ???
            val oldNamer = namer

            if (sym.isInstrumented) {
              namer = analyzer.newNamer(namer.context.make(tree, sym.owner, sym.owner.info.decls))
              if (settings.Yeventsdebug.value) {
                  println("Transform of observable class called for :  " + name)
              }
              val pos = sym.pos

              // Create newobj
              val parents = List(genAllObjectTpt(TypeTree(sym.tpe)), TypeTree(ScalaObjectClass.tpe))
              val newobj = atPos(pos)(ModuleDef ( NoMods, 
                                      name+"$all", 
                                      Template(parents,emptyValDef, NoMods, List(Nil), List(Nil), Nil, pos)
                                      ))
              // Enter the objects in the symbol table and type it
              namer.enterSyntheticSym(newobj)
              obsobjects = localTyper.typed(newobj).asInstanceOf[ModuleDef] :: obsobjects

              // Same as in package in case of class in class
              val oldobsobjects = obsobjects
              obsobjects = List()
              val tclazz = super.transform(cd).asInstanceOf[ClassDef]

              // Add self to allobjects in the constructor by modifying template
              // While we are modifying template, we add the obsobjects
              var template = tclazz.impl
              val apply = atPos(pos)(localTyper.typed(Apply(Select(Ident(name+"$all"),newTermName("register")),List(This(sym)))))
              template = treeCopy.Template(template, template.parents,
                                        template.self, obsobjects ::: (apply :: template.body))

              //Return the classdef with the new template
              val result = treeCopy.ClassDef(tclazz, tclazz.mods, tclazz.name,
                                                      tclazz.tparams, template)
              namer = oldNamer
              obsobjects = oldobsobjects
              result
            } else {
              // Same as in package in case of class in class
              val oldobsobjects = obsobjects
              obsobjects = List()
              val tclazz = super.transform(cd).asInstanceOf[ClassDef]
              var template = tclazz.impl
              template = treeCopy.Template(template, template.parents,
                         template.self, obsobjects ::: template.body)
              //Return the classdef with the new template
              val result = treeCopy.ClassDef(tclazz, tclazz.mods, tclazz.name,
                                                      tclazz.tparams, template)
              obsobjects = oldobsobjects
              result
            }
          case _ => super.transform(tree)
        }
    }
 }

}
// vim: set ts=4 sw=4 et:
