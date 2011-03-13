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

						val leftHs : Tree = body match
						{
							case as @ Assign(lhs, rhs) =>								
								lhs
							case _ =>
								body
						}
						
						val typedLhs = localTyper.typed(leftHs)
						
//println("typed: " + typedLhs.tpe)
						
//println("leftHs.symbol? " + leftHs.symbol)
//println("leftHs.type? " + leftHs.symbol.tpe)
            val tupledGenericParam = vparams.flatten.map(vd => vd.tpt)
//println("tupledGenericParam: " + tupledGenericParam)
            
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
         
            val beforeEvName = buildBeforeSetEventName(sym)
            val afterEvName = buildAfterSetEventName(sym)
            // Event Callback params: (currentVal, newVal)
            var beforeEv = genEvent(dd, modifiers, beforeEvName, genImperativeEventTpt(tupledGenericParam ::: List(typedLhs)), newBeforeSetEvent(tupledGenericParam ::: List(typedLhs)), pos)
            // Event Callback params: (newVal) --> oldVal not reachable because afterEvent compiled after body(with reassignment)
            var afterEv = genEvent(dd, modifiers, afterEvName, genImperativeEventTpt(tupledGenericParam),
                                     newAfterSetEvent(tupledGenericParam), pos)
              
            // enter the declaration of the events in the class declarations 
            namer.enterSyntheticSym(beforeEv)
            namer.enterSyntheticSym(afterEv)
              
            // type the events
            def typeEvent(ev: ValDef) = localTyper.typed(ev).asInstanceOf[ValDef]
            beforeEv = typeEvent(beforeEv)
            afterEv = typeEvent(afterEv)

            // list of parameters
            val args = vparams.flatten[global.ValDef].map(vd => Ident(vd.name))
            val evArgs = 
              if(args.size == 0) {
                List(Literal(()))
              } else
               args
                  
            // the body is a block triggering before, calling the implementation,
            // triggering after and returning the result
            val tupledEvArgs = evArgs.head
              //if(evArgs.size > 1)
                //genTupleTerm(evArgs)
              //else
                //evArgs.head
            //println("tupledEvArgs: " + tupledEvArgs)  
            
            var wrapperBody =
                atPos(pos)(Block(
                          Apply(
                              Ident(beforeEvName),
                              leftHs :: List(tupledEvArgs)) ::
                          body ::
                          Nil,
                          Apply(
                              Ident(afterEvName),
                              tupledEvArgs :: Nil
                          )
                      ))
            synthesized = beforeEv :: afterEv :: synthesized

        		localTyper.typed(treeCopy.DefDef(dd, mods, name, tparams, vparams, retType, wrapperBody).setType(null))
        case _ => super.transform(tree)
      }
    }

    // generate an imperative event declaration initialized with the given body
    private def genEvent(tree: DefDef, modifiers: Modifiers, name: Name, tpt: Tree, body: Tree, pos: Position) = {
      
      val flags = modifiers | LAZY | EVENT | (if(settings.Yeventsdebug.value) 0 else SYNTHETIC)
      
      val event = ValDef(flags, name, tpt, body)
      atPos(pos)(event)
    }    
  }

}
