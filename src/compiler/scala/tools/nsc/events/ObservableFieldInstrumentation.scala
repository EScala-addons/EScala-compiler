package scala.tools.nsc
package events

import transform._
import symtab._
import Flags._
import util.EventUtil
import typechecker._

/**
 * This class allows to intrument the observable fields.
 *
 * @author Acun Guersoy
 */
abstract class ObservableFieldInstrumentation extends ObservableUtil {

  import global._
  import definitions._

  val phaseName: String = "observablefields"

  // the Observable class represents the instrumented method
  protected var namer: analyzer.Namer = null

  protected var toInstrument: List[Symbol] = Nil
    
  def newTransformer(unit: CompilationUnit): Transformer = {
    new ObservablesLift(unit)
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

  class ObservablesLift(unit: CompilationUnit) extends TypingTransformer(unit) { 
        
    import symtab.Flags._
    
    private var clazz: ClassDef = null
    private var synthesized: List[Tree] = null
    
    object duplicator extends {
      val global: ObservableFieldInstrumentation.this.global.type = ObservableFieldInstrumentation.this.global
    } with Duplicators

    /** 
     * Perform the following transformations:
     * <ul>
     *  <li>
     *    for each observable method, transform it in an instrumented method.
     *    Different case are possible for a method m:
     *    <ul>
     *      <li>
     *        the method does not override any instrumented method:
     *        <ul>
     *          <li>
     *            an internal method with the original implementation is generated:
     *            <pre>
     *            protected def m$impl(...) = { body }
     *            </pre>
     *          </li>
     *          <li>
     *            two events are generated as class members m$before and m$after
     *          </li>
     *          <li> 
     *            the original method triggers the before event, call the implementation
     *            and triggers the after event:
     *            <pre>
     *            &lt;observable&gt; def m(...) = {
     *              m$before(...)
     *              val res = m$impl(...)
     *              m$after(...)
     *              res
     *            }
     *            </pre>
     *          </li>
     *        </ul>
     *      </li>
     *      <li>
     *        the method overrides an already instrumented method:
     *        only the implementation method is generated:
     *        <pre>
     *        override protected def m$impl(...) = { body }
     *        </pre>
     *      </li>
     *    </ul>
     *  </li>
     * </ul>
     */
    override def transform(tree: Tree): Tree = {
    	// TODO!! Eigene Routine, wo before und afterSet Event in Setter reingeschrieben wird
      // exec nicht notwendig, stattdessen einfach setterInhalt zwischen beide EventRefs schreiben
      val sym = tree.symbol      
      tree match {
        case pd: PackageDef =>
          val oldNamer = namer
          namer = analyzer.newNamer(namer.context.make(tree, sym, sym.info.decls))
          val res = super.transform(tree)
          namer = oldNamer
          res
        case cd: ClassDef => 
          val oldsynthesized = synthesized
          synthesized = List()
          val oldclazz = clazz
          clazz = cd

          // transform the class body
          val oldNamer = namer
          namer = analyzer.newNamer(namer.context.make(tree, sym, sym.info.decls))
          val tclazz = super.transform(cd).asInstanceOf[ClassDef]

          // add the synthesized methods
          var template = tclazz.impl
          template = treeCopy.Template(template, template.parents,
                     template.self, synthesized ::: template.body)

          namer = oldNamer

          // switch the implementation
          val result = treeCopy.ClassDef(tclazz, tclazz.mods, tclazz.name,
                       tclazz.tparams, template)

          if(!synthesized.isEmpty) {
            // the class has now concrete element, reset the <interface> flag if it was set
            clazz.symbol.resetFlag(INTERFACE)
          }

          clazz = oldclazz
          synthesized = oldsynthesized
          result
        
        case dd @ DefDef(mods, name, tparams, vparams, retType, body) 
        	if(sym.isSetter && sym.isInstrumented) =>
        		val pos = sym.pos

						
            var genericParam = vparams.flatten.map(vd => vd.tpt)
//println("genericParam: " + genericParam)
            
            
            // toDelete? case never existing, because setter always got one param (only type needed)
            if(genericParam.isEmpty) {
              // Unit as generic parameter
              genericParam = List(Ident(newTypeName("Unit")))
            }
            
            // always else-case? then no if needed
            val tupledGenericParam =
              //if(genericParam.size > 1)
                //genTupleType(genericParam)
              //else
                genericParam
//println("tupledGenericParam: " + tupledGenericParam);
            
            // the event modifiers
              val modifiers = 
                  (dd.mods & ~OVERRIDE & ~OBSERVABLE & ~DEFERRED & ~INSTRUMENTED) | (
                    if(!sym.isObservable && !sym.isPrivate)
                      // only instrumented => protected events
                      PROTECTED //| LOCAL
                    else
                      // the method is observable or private => same visibility
                      FINAL
                  )
                  
println("SYMBOL in instrumentation: " + sym)

              val beforeEvName = buildBeforeEventName(sym)
              val afterEvName = buildAfterEventName(sym)
              println("sym: ___________________ " + sym)
              println("name: ___________________ " + name)
              println("beforeEvName: ------------------- " + beforeEvName)
              //println("afterEvName: ------------------- " + afterEvName)
              println("genEvent with params (dd, modifiers, beforeEvName): __________ " + dd + ", " + modifiers + ", " + beforeEvName)
              println("")
              
              
              var beforeEv = genEvent(dd, modifiers, beforeEvName, genImperativeEventTpt(tupledGenericParam), newBeforeSetEvent(tupledGenericParam), pos)
              var afterEv = genEvent(dd, modifiers, afterEvName, genImperativeEventTpt(tupledGenericParam ::: List(retType)),
                                     newAfterSetEvent(tupledGenericParam ::: List(retType)), pos)
              //var execEv = genEvent(dd, modifiers, execEvName, genExecutionEventTpt(tupledGenericParam, tupledGenericParam ::: List(retType)),
                                    //newExecutionEvent(tupledGenericParam, tupledGenericParam ::: List(retType)), pos)
              // enter the declaration of the events in the class declarations 
              // AG: -> namer references class?
              namer.enterSyntheticSym(beforeEv)
              namer.enterSyntheticSym(afterEv)

              // type the events
              def typeEvent(ev: ValDef) = localTyper.typed(ev).asInstanceOf[ValDef]
              beforeEv = typeEvent(beforeEv)
              afterEv = typeEvent(afterEv)
              
              //println("AFTER TYPING created Events:")
              //println("beforeEvent: " + beforeEv)
              //println("afterEvent: " + afterEv)
              
              // the wrapper method

              // list of parameters
              val args = vparams.flatten[global.ValDef].map(vd => Ident(vd.name))
              val evArgs = 
                if(args.size == 0) {
                  List(Literal(()))
                } else
                  args

							println("given args, size: " + args + ", " + args.size)
							


              // the body is a block triggering before, calling the implementation,
              // triggering after and returning the result
              val tupledEvArgs =
                if(evArgs.size > 1)
                  genTupleTerm(evArgs)
                else
                  evArgs.head
                  
             println("tupledEvArgs (evArgs.size >1?: " + tupledEvArgs)
              
              val wrapperBody =
                  // AG: using beforeEv.symbol instead of beforeEvName because 
                  // "$eq" becomes "=" after namer.enterSyntheticSym(beforeEv) so referencing not correct?
                  atPos(pos)(Block(
                            Apply(
                                Ident(beforeEv.symbol),
                                tupledEvArgs :: Nil) ::
                            body ::
                            Nil,
                            Apply(
                                Ident(afterEv.symbol),
                                tupledEvArgs :: List(Literal(()))
                            )
                        ))
                
                
                synthesized = beforeEv :: afterEv :: synthesized

        	treeCopy.DefDef(dd, mods, name, tparams, vparams, retType, wrapperBody)
        	//super.transform(tree)
       	
        case _ => super.transform(tree)
      }
    }

    // generate an imperative event declaration initialized with the given body
    private def genEvent(tree: DefDef, modifiers: Modifiers, name: Name, tpt: Tree, body: Tree, pos: Position) = {
      
      val flags = modifiers | LAZY | EVENT | (if(settings.Yeventsdebug.value) 0 else SYNTHETIC)
      
      val event = ValDef(flags, name, tpt, body)
      atPos(pos)(event)
    }

/*    
    def buildBeforeEventName(meth: Symbol) =
    internalBuild(meth, "$before")

  	def buildAfterEventName(meth: Symbol) =
    internalBuild(meth, "$after")
    
    private def internalBuild(meth: Symbol, suffix: String) = {
    meth.tpe match {
      case mt @ MethodType(params, retType) =>
        // build the string representing the parameters
        val paramString = mt.paramTypes.foldLeft("")(
          (prefix, pt) => prefix + "$" + pt.typeSymbol.rawname
        )
println("EVENT UTIL, paramString: " + paramString)
println("RAW PARAM meth (sym): " + meth)
println("final NAME parts : " + meth.rawname + " -- " + paramString + " -- " + suffix)
println("")
        // and the final name
        meth.name + paramString + suffix
      case _ => ""
    }
  }
*/    
    
    
  }

}
