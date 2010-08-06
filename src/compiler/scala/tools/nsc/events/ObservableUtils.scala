package scala.tools.nsc
package events

trait ObservableUtil {

  val global: Global
  import global._
  import symtab.Flags._

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
    val dependentType =
      Select(
        Ident(exec),
        newTypeName("BeforeExecution"))
    val x = nme.ANON_CLASS_NAME.toTypeName
    Block(
      List(
        ClassDef(
          Modifiers(FINAL), x, Nil,
          Template(genImperativeEventTpt(tparams) :: dependentType :: Nil, emptyValDef, NoMods, List(Nil), List(Nil), Nil, NoPosition)
        )
      ),
      New(
        Ident(x),
        List(Nil)
      )
    )
  }

  protected[events] def newAfterExecEvent(tparams: List[Tree], exec: Name) = {
    val dependentType =
      Select(
        Ident(exec),
        newTypeName("AfterExecution"))
    val x = nme.ANON_CLASS_NAME.toTypeName
    Block(
      List(
        ClassDef(
          Modifiers(FINAL), x, Nil,
          Template(genImperativeEventTpt(tparams) :: dependentType :: Nil, emptyValDef, NoMods, List(Nil), List(Nil), Nil, NoPosition)
        )
      ),
      New(
        Ident(x),
        List(Nil)
      )
    )
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
