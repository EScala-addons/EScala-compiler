package scala.tools.nsc
package events

import transform._
import symtab._
import Flags._
import util.EventUtil
import typechecker._

/**
 * This class allows to intrument the observable methods.
 *
 * @author Lucas Satabin
 */
abstract class ObservableInstrumentation extends Transform 
                                         with EventUtil
                                         with TypingTransformers
                                         with ObservableUtil {

  import global._
  import definitions._

  val phaseName: String = "observables"

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
      val global: ObservableInstrumentation.this.global.type = ObservableInstrumentation.this.global
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
              if sym.isInstrumented =>
                              
            val pos = sym.pos
            
            // generate the implementation method
            val newName = buildImplMethodName(sym)
            

            // indicates whether this overrides another observable method
            val overrideObs = isSuperObservable(sym, clazz.symbol)
            // indicates whether this overrides an instrumented method (not necessary observable)
            val overrideInstr = isSuperInstrumented(sym, clazz.symbol)
            
            // the flags for the implementation method
            var implMod = Modifiers(PROTECTED | IMPLEMENTATION | (if(settings.Yeventsdebug.value) 0 else SYNTHETIC))
            if (overrideInstr && sym.isOverride) {
              implMod = implMod | OVERRIDE  
            }
            if (mods.isDeferred) {
              implMod = implMod | DEFERRED  
            }
            
            // enter and type the implementation method
            val tparamsImpl = tparams.map(
                tp => TypeDef(tp.mods, tp.name, tp.tparams, tp.rhs))
            val vparamsImpl = vparams.map(
                vl => vl.map(
                    vp => ValDef(vp.mods, vp.name, vp.tpt, vp.rhs)
                )
            )

            var impl = atPos(pos)(DefDef(implMod, newName,
                tparamsImpl, vparamsImpl, retType.copyAttrs(retType), transform(body)))

            namer.enterSyntheticSym(impl)
            // set the implemented method symbol
            impl.symbol.implementedMethod = sym

            // retype the method
            impl = duplicator.retyped(localTyper.context1.asInstanceOf[duplicator.Context], impl, clazz.symbol, clazz.symbol, scala.collection.immutable.Map.empty[Symbol,Type]).asInstanceOf[DefDef]

            // reference the parameters of the implementation method
            val oldSym = vparams.flatten.map(
              vp => vp.symbol
            )
            val newSym = vparamsImpl.flatten.map(
              vp => vp.symbol
            )
            impl = new TreeSymSubstituter(oldSym, newSym)(impl)
            new ChangeOwnerTraverser(dd.symbol, impl.symbol).traverse(impl)

            var genericParam = vparams.flatten.map(vd => vd.tpt)
            if(genericParam.isEmpty) {
              // Unit as generic parameter
              genericParam = List(Ident(newTypeName("Unit")))
            }
            val tupledGenericParam =
              if(genericParam.size > 1)
                genTupleType(genericParam)
              else
                genericParam
          
            if(!overrideInstr){
              // the super method was not instrumented (neither observable nor instrumented)
              // generate the events and the wrapper

              // the events

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

              val beforeEvName = buildBeforeEventName(sym)
              val afterEvName = buildAfterEventName(sym)
              var beforeEv = genEvent(dd, modifiers, beforeEvName, genImperativeEventTpt(tupledGenericParam), newBeforeExecEvent(tupledGenericParam), pos)
              var afterEv = genEvent(dd, modifiers, afterEvName, genImperativeEventTpt(tupledGenericParam ::: List(retType)),
                                     newAfterExecEvent(tupledGenericParam ::: List(retType)), pos)
              // enter the declaration of the events in the class declarations
              namer.enterSyntheticSym(beforeEv)
              namer.enterSyntheticSym(afterEv)

              // type the events
              def typeEvent(ev: ValDef) = localTyper.typed(ev).asInstanceOf[ValDef]
              beforeEv = typeEvent(beforeEv)
              afterEv = typeEvent(afterEv)
              
              // the wrapper method

              // list of parameters
              val args = vparams.flatten[global.ValDef].map(vd => Ident(vd.name))
              val evArgs = 
                if(args.size == 0) {
                  List(Literal(()))
                } else
                  args

              // the body is a block triggering before, calling the implementation,
              // triggering after and returning the result
              val tupledEvArgs =
                if(evArgs.size > 1)
                  genTupleTerm(evArgs)
                else
                  evArgs.head
              
              val wrapperBody =
                if(retType.tpe == definitions.UnitClass.tpe) {
                  // the return type is unit, do not save the result
                  atPos(pos)(Block(
                            Apply(
                                Ident(beforeEvName),
                                tupledEvArgs :: Nil) ::
                            Apply(
                                Ident(newName),
                                args) ::
                            Nil,
                            Apply(
                                Ident(afterEvName),
                                tupledEvArgs :: List(Literal(()))
                            )
                        ))
                } else {
                  // the return type is not unit, save the result
                  atPos(pos)(Block(
                              Apply(
                                Ident(beforeEvName),
                                tupledEvArgs :: Nil) ::
                              ValDef(NoMods,
                                newTermName("res"),
                                retType,
                                Apply(
                                    Ident(newName),
                                    args)) ::
                              Apply(
                                Ident(afterEvName),
                                tupledEvArgs :: List(Ident("res"))
                              ) ::
                              Nil,
                              Ident(newTermName("res"))
                        ))
                }
              
              
                        
                        
              // the symbol and type information
              sym.resetFlag(DEFERRED)
              var wrapperMeth = atPos(pos)(DefDef(mods, name,
                     tparams,
                     vparams,
                     retType, wrapperBody)).setSymbol(sym)
              wrapperMeth = localTyper.typed(wrapperMeth).asInstanceOf[DefDef]
              
              // add to the list of synthesized members
              synthesized = wrapperMeth :: beforeEv :: afterEv :: synthesized
            } else if(!overrideObs && overrideInstr && sym.isObservable) {
              // the method overrides an already internally instrumented method
              // makes the protected events visible

              // generate the events

              // the event modifiers
              val modifiers = (dd.mods & ~OBSERVABLE & ~DEFERRED & ~INSTRUMENTED) | FINAL

              val beforeEvName = buildBeforeEventName(sym)
              val afterEvName = buildAfterEventName(sym)
              var beforeEv = genEvent(dd, modifiers, beforeEvName, genImperativeEventTpt(tupledGenericParam), superBeforeExec(sym.name), pos)
              var afterEv = genEvent(dd, modifiers, afterEvName, genImperativeEventTpt(tupledGenericParam ::: List(retType)), superAfterExec(sym.name), pos)

              // enter the declaration of the events in the class declarations
              namer.enterSyntheticSym(beforeEv)
              namer.enterSyntheticSym(afterEv)

              // type the events
              beforeEv = localTyper.typed(beforeEv).asInstanceOf[ValDef]
              afterEv = localTyper.typed(afterEv).asInstanceOf[ValDef]

              // the wrapper method simply calls the super instrumented method
              // handle curried function call
              val applies = vparams.foldLeft[Tree](Select(Super(tpnme.EMPTY, tpnme.EMPTY), name)) {
                (base, vds) => Apply(base, vds.map(vd => Ident(vd.name)))
              }
              val wrapperBody = atPos(pos)(applies)

              sym.resetFlag(DEFERRED).setFlag(OVERRIDE)
              var wrapperMeth = atPos(pos)(DefDef(mods, name,
                     tparams,
                     vparams,
                     retType, wrapperBody)).setSymbol(sym)

              wrapperMeth = localTyper.typed(wrapperMeth).asInstanceOf[DefDef]

              // add to the list of synthesized members
              synthesized = wrapperMeth :: beforeEv :: afterEv :: synthesized

            } else {
               // remove the original method from the symbol table
               clazz.symbol.info.decls.unlink(sym)
                
            }
            // return the implementation
            
                                
            // the result
            impl
                
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
// vim: set ts=4 sw=4 et:
