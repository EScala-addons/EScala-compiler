package scala.tools.nsc
package events

import transform._
import ast._
import symtab._
import Flags._
import util.EventUtil
import typechecker._

import scala.collection.mutable.{HashMap,MultiMap,Set}

/**
 * This phase marks the method that will be instrumented. It does not do the instrumentation yet.
 *
 * @author Lucas Satabin
 */
abstract class InstrumentMarker extends Transform
                       with TypingTransformers
                       with EventUtil {

  import global._
  import definitions._

  val phaseName: String = "instrumentmarker"

  private var namer: analyzer.Namer = null

  /** This multi map contains the classes referencing a method m as instrumented m.
   *  The key is the method symbol, the value is the class needing to instrument the method.
   *  The class is the highest found class in the hierarchy requiring instrumentation
   *  If we have following hierarchy
   *  
   *  <pre>
   *  class A { def m() { ... } }
   * 
   *  class B extends A {
   *    evt e = afterExec(this.m)
   *  }
   *
   *  class C extends B {
   *    evt e2 = beforeExec(this.m)
   *  }
   *
   *  class D extends A {
   *    evt e = beforeExec(this.m)
   *  }
   *  </pre>
   *
   *  the map contains m as key with two values: B and D
   */
  private var toInstrument: MultiMap[Symbol, Symbol] = new HashMap[Symbol, Set[Symbol]] with MultiMap[Symbol, Symbol]
    
  def newTransformer(unit: CompilationUnit): Transformer = new MetaTransformer(unit)

  /** Create a new phase which applies transformer */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      namer = analyzer.newNamer(analyzer.rootContext(unit))
      newTransformer(unit) transformUnit unit
    }
  }

  class MetaTransformer(unit: CompilationUnit) extends Transformer {
    override def transformUnit(unit: CompilationUnit) = {
      // collect all the method to implicitly instrument
      referencesCollecter.traverse(unit.body)
      if(settings.Yeventsdebug.value)
        println("methods to internally instrument (method, classes using reference to their implicit events): " + toInstrument.mkString("\n", "\n", "\n================="))
      // for each class symbol, contains the list of methods that are to instrument in this class
      val methodsToInstrument: MultiMap[Symbol, Symbol] = new HashMap[Symbol, Set[Symbol]] with MultiMap[Symbol, Symbol]
      // only one class per method per hierarchy must mark the method to be instrumented
      toInstrument.foreach(onePerHierarchy(methodsToInstrument))
      if(settings.Yeventsdebug.value)
        println("method to instrument in each class: " + methodsToInstrument.mkString("\n", "\n", "\n================="))
      new InstrumentMarkerTransformer(unit, methodsToInstrument).transformUnit(unit)
    }

    def onePerHierarchy(toFill: MultiMap[Symbol, Symbol])(meth: Symbol, classes: Set[Symbol]) {
      // first filter the classes that really need to instrument a method
      val filtered = classes.foldLeft[Set[Symbol]](Set.empty)((prev, sym) => {
          if(settings.Yeventsdebug.value)
            println("inspecting " + sym + ". already in set: " + prev)
          if(!prev.exists(sym.tpe <:< _.tpe)) {
            if(settings.Yeventsdebug.value)
              println("adding " + sym + " to the set and removing " + prev.filter(_.tpe <:< sym.tpe))
            // add the class to the set and remove subclasses from the set
            prev.filterNot(_.tpe <:< sym.tpe) + sym
          } else
            prev
        })
      // update the map to fill
      filtered.foreach(toFill.addBinding(_, meth))
    }

  }

  /**
   * This traverser collects the references to exec event for methods that are not marked as observable
   */
  object referencesCollecter extends Traverser {
    var currentThis: Symbol = NoSymbol
    override def traverse(tree: Tree) {
      tree match {
        case cd: ClassDef => 
          if(settings.Yeventsdebug.value)
            println("traversing " + cd.symbol)
          currentThis = cd.symbol
          super.traverse(tree)
        case ExecEvent(kind, meth) if(!meth.symbol.isInstrumented) =>
          // so far we are sure that a non instrumented referenced method is defined in this class or in a parent class
          if(settings.Yeventsdebug.value)
            println("referencing method " + meth.symbol + " in class " + currentThis)
          val declaringClass = meth.symbol.owner
          toInstrument.addBinding(meth.symbol, currentThis)
        case _ => super.traverse(tree)
      }
    }
  }

  class InstrumentMarkerTransformer(unit: CompilationUnit, methodsInClass: MultiMap[Symbol, Symbol]) extends TypingTransformer(unit) {

    var namer = analyzer.newNamer(analyzer.rootContext(unit))

    val allMethods = methodsInClass.values.flatMap(s => s)

    var clazz: Symbol = NoSymbol

    override def transform(tree: Tree) = {
      tree match {
        case cd @ ClassDef(mods, name, tparams, template @ Template(parents, self, body)) if methodsInClass.contains(cd.symbol) =>
          // set the instrument flag and override the method if necessary
          clazz = cd.symbol
          namer = analyzer.newNamer(namer.context.make(tree, clazz, clazz.info.decls))

          val methods = methodsInClass(cd.symbol)
          // the synthesized overridden methods if any
          var synthesized: List[DefDef] = Nil
          methods.foreach(markMethod(_, clazz) match {
              case Some(m) => synthesized = m :: synthesized
              case None => /* do nothing */
            })
          // copy the method with the new overridden methods
          treeCopy.ClassDef(cd, mods, name, tparams, treeCopy.Template(template, parents, self, super.transformTrees(body) ::: synthesized))
        case dd: DefDef if !dd.symbol.isConstructor && !dd.symbol.hasFlag(INSTRUMENTED)
                           && (allMethods.exists(anySuperSymbol(dd.symbol, clazz) ==) ||
                              (isSuperInstrumented(dd.symbol, dd.symbol.owner)) && !isSuperObservable(dd.symbol, dd.symbol.owner)) =>
          // if the method overrides an isntrumented method (but not observable) set the instrumented flag too
          // if the overridden method was declared as `observable'
          if(settings.Yeventsdebug.value)
            println(dd + " overrides instrumented method")
          dd.symbol.setFlag(INSTRUMENTED)
          super.transform(tree)
        case _ => super.transform(tree)
      }
    }

    /** Mark the method as instrumented.
     *  @returns Some method if the method was overridden, None otherwise
     */
    def markMethod(meth: Symbol, clazz: Symbol): Option[DefDef] = {
      if(meth.owner == clazz) {
        // just set the flag
        meth setFlag INSTRUMENTED
        None
      } else
        // override the method
        Some(generateMethod(meth, clazz))
    }

    /** Generates a new method overriding the given one and mark it as instrumented.
     */
    private def generateMethod(meth: Symbol, clazz: Symbol): DefDef = {
      if(settings.Yeventsdebug.value)
        println("overriding " + meth + " declared in " + meth.owner + " in " + clazz)
      // override the method defined in a parent class, marking it with the INSTRUMENTED flag
      // it simply calls the super method, instrumentation is done later

      val newSym = meth.cloneSymbol(clazz)        
      if (meth.isDeferred)
        newSym.setFlag(DEFERRED | INSTRUMENTED)
      else
        newSym.setFlag(OVERRIDE | INSTRUMENTED)
      namer.enterInScope(newSym)

      if (meth.isDeferred)
        DefDef(newSym, EmptyTree)
      else
        DefDef(newSym, genSuperCall(meth) _)
    }

    private def genSuperCall(meth: Symbol)(paramss: List[List[Symbol]]) = {
      val superMeth = Select(Super(nme.EMPTY, nme.EMPTY), meth)
      paramss.foldLeft[Tree](superMeth)((base, args) => Apply(base, args.map(s => Ident(s))))
    }

  }

}

// vim: set ts=4 sw=4 et:
