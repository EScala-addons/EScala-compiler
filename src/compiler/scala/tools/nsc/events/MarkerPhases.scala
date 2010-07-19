package scala.tools.nsc
package events

import symtab._

import scala.collection.mutable.{HashMap,MultiMap,Set}

trait MarkerPhases {

  val global: Global
  import global._

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
  protected[events] var toInstrument: MultiMap[Symbol, Symbol] = new HashMap[Symbol, Set[Symbol]] with MultiMap[Symbol, Symbol]

  /**
   * This phase traverses the compilation units and collects all the methods that are to locally instrument
   */
  object referencesCollecter extends SubComponent {
    val global: MarkerPhases.this.global.type = MarkerPhases.this.global
    val phaseName = "referencescollecter"
    val runsAfter = List[String]("typer")
    val runsRightAfter = None

    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
    
      var currentThis: Symbol = NoSymbol
      
      override def run = {
        // we first traverse all the compilation units
        super.run
        // then we eliminate the collected methods that are useless
        if(settings.Yeventsdebug.value)
          println("methods to internally instrument (method, classes using reference to their implicit events): " + toInstrument.mkString("\n", "\n", "\n================="))
        // for each class symbol, contains the list of methods that are to instrument in this class
        val methodsToInstrument: MultiMap[Symbol, Symbol] = new HashMap[Symbol, Set[Symbol]] with MultiMap[Symbol, Symbol]
        // only one class per method per hierarchy must mark the method to be instrumented
        toInstrument.foreach(onePerHierarchy(methodsToInstrument))
        toInstrument = methodsToInstrument
        if(settings.Yeventsdebug.value)
          println("method to instrument in each class: " + methodsToInstrument.mkString("\n", "\n", "\n================="))
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

      def apply(unit: CompilationUnit) {
        traverser.traverse(unit.body)
      }

      object traverser extends Traverser {
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
    }
  }

  object instrumentMarker extends InstrumentMarker {
    val global: MarkerPhases.this.global.type = MarkerPhases.this.global
    val phaseName = "instrumentmarker"
    val runsAfter = List[String]("referencescollecter")
    val runsRightAfter = None

    def newTransformer(unit: CompilationUnit): Transformer = new InstrumentMarkerTransformer(unit, toInstrument)

    /** Create a new phase which applies transformer */
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

    /** The phase defined by this transform */
    class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
      def apply(unit: global.CompilationUnit): Unit = {
        namer = analyzer.newNamer(analyzer.rootContext(unit))
        newTransformer(unit) transformUnit unit
      }
    }

  }

}

// vim: set ts=2 sw=2 et:
