package scala.tools.nsc
package events

import util.EventUtil
import transform._

import ast.parser.TreeBuilder

trait ObservableUtil extends Transform 
                     with EventUtil
                     with TypingTransformers{
	self =>
	
  val global: Global
  import global._
  import symtab.Flags._

  object treeBuilder extends TreeBuilder {
    val global: self.global.type = self.global
    private var cnt = 0

    def freshName(prefix: String) = {
      cnt += 1
      newTermName(prefix + (cnt - 1) + "$")
    }
    override def freshName() = freshName("events$")
    def o2p(offset: Int) = NoPosition
    def r2p(start: Int, point: Int, end: Int) = NoPosition
  }

  import treeBuilder._

  protected[events] def genTupleType(l: List[Tree])=
    List(AppliedTypeTree(Select(Ident("scala"), newTypeName("Tuple" + l.size)), l))
  protected[events] def genTupleTerm(l: List[Tree])=
    Apply(Select(Ident("scala"), newTermName("Tuple" + l.size)), l)

  // creates a tree creating a new imperative event
  protected[events] def newImperativeEvent(tparams: List[Tree]) = 
    Apply(
      Select(
        New(
          genImperativeEventTpt(tparams)
        ),
        nme.CONSTRUCTOR),
      Nil)

  protected[events] def newBeforeExecEvent(tparams: List[Tree], exec: Name) = {
  	println("using name for new beforeExec: " + exec)
    val dependentType =
      Select(
        Ident(exec),
        newTypeName("BeforeExecution"))
    println("\n+++++++++++++++")
    println(makeNew(genImperativeEventTpt(tparams) :: dependentType :: Nil, emptyValDef, Nil, List(Nil), NoPosition, NoPosition))
    println("\n222222222222222")
    println(makeNew(genImperativeEventTpt(tparams) :: Nil, emptyValDef, Nil, List(Nil), NoPosition, NoPosition))
    println("\n+++++++++++++++")
    makeNew(genImperativeEventTpt(tparams) :: dependentType :: Nil, emptyValDef, Nil, List(Nil), NoPosition, NoPosition)
  }

  protected[events] def newAfterExecEvent(tparams: List[Tree], exec: Name) = {
  	println("using name for new afterExec: " + exec)
    val dependentType =
      Select(
        Ident(exec),
        newTypeName("AfterExecution"))
    makeNew(genImperativeEventTpt(tparams) :: dependentType :: Nil, emptyValDef, Nil, List(Nil), NoPosition, NoPosition)
  }
  
  protected[events] def newBeforeSetEvent(tparams: List[Tree]) = {
    val dependentType =
    	newTypeName("BeforeSet")
      /*Select(
        Ident(exec),
        newTypeName("BeforeSet"))*/
    makeNew(genImperativeEventTpt(tparams) :: dependentType :: Nil, emptyValDef, Nil, List(Nil), NoPosition, NoPosition)
  }

  protected[events] def newAfterSetEvent(tparams: List[Tree]) = {
    val dependentType =
    	newTypeName("AfterSet")
      /*Select(
        Ident(exec),
        newTypeName("AfterSet"))*/
    makeNew(genImperativeEventTpt(tparams) :: dependentType :: Nil, emptyValDef, Nil, List(Nil), NoPosition, NoPosition)
  }
  
  protected[events] def newExecutionEvent(beforeTparams: List[Tree], afterTparams: List[Tree]) =
    Apply(
      Select(
        New(
          genExecutionEventTpt(beforeTparams, afterTparams)
        ),
        nme.CONSTRUCTOR),
      Nil)

  private def tupleize(params: List[Tree]) = 
    if(params.size > 1)
      genTupleType(params)
    else
      params

  protected[events] def genImperativeEventTpt(tparams: List[Tree]) = {
    val generics = tupleize(tparams)
    AppliedTypeTree(
      Select(
        Select(
          Ident("scala"),
          newTermName("events")
        ),
        newTypeName("ImperativeEvent")
      ),
    generics)
  }

  protected[events] def genExecutionEventTpt(beforeTparams: List[Tree], afterTparams: List[Tree]) = {
    val beforeG = tupleize(beforeTparams)
    val afterG = tupleize(afterTparams)
    AppliedTypeTree(
      Select(
        Select(
          Ident("scala"),
          newTermName("events")
        ),
        newTypeName("ExecutionEvent")
      ),
    beforeG ::: afterG)
  }

  protected[events] def genIntervalEventTpt(beforeTparams: List[Tree], afterTparams: List[Tree]) = {
    val beforeG = tupleize(beforeTparams)
    val afterG = tupleize(afterTparams)
    AppliedTypeTree(
      Select(
        Select(
          Ident("scala"),
          newTermName("events")
        ),
        newTypeName("IntervalEvent")
      ),
    beforeG ::: afterG)
  }

  protected[events] def superBeforeExec(meth: Name) =
    ExecEvent(
      BeforeExec(),
      Select(
        // TODO better handles in which mixin it was?
        Super(nme.EMPTY, nme.EMPTY),
        meth
      )
    )

  protected[events] def superAfterExec(meth: Name) =
    ExecEvent(
      AfterExec(),
      Select(
        // TODO better handles in which mixin it was?
        Super(nme.EMPTY, nme.EMPTY),
        meth
      )
    )
}

// vim: set ts=2 sw=2 et:
