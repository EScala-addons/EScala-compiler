/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$
                                                                      
package scala.tools.nsc.matching

import scala.tools.nsc.util.Position

/** contains many helper methods that build trees...some of these currently
 *  unused, since were for regexp matching.
 *
 *  @author Burak Emir
 */
trait CodeFactory { 
  self: transform.ExplicitOuter =>

  import global._

  import definitions._             // standard classes and methods
  import typer.typed               // methods to type trees
  import posAssigner.atPos         // for filling in tree positions 


  final def typedValDef(x:Symbol, rhs:Tree) = typed{ValDef(x, typed{rhs})}

  final def mk_(tpe:Type) = Ident(nme.WILDCARD) setType tpe

  final def targetLabel(owner: Symbol, pos: Position, name:String, argtpes:List[Type], resultTpe: Type) = 
    owner.newLabel(pos, name).setInfo(new MethodType(argtpes, resultTpe))

  final def targetParams(subst:List[Pair[Symbol,Symbol]]) = subst map { 
    case (v,t) => ValDef(v, {
      v.setFlag(symtab.Flags.TRANS_FLAG); 
      if(t.tpe <:< v.tpe) typed{Ident(t)} 
      else if(v.tpe <:< t.tpe) typed{gen.mkAsInstanceOf(Ident(t),v.tpe)} // refinement
      else {
        //Console.println("internal error, types don't match: pattern variable "+v+":"+v.tpe+" temp "+t+":"+t.tpe)
        error("internal error, types don't match: pattern variable "+v+":"+v.tpe+" temp "+t+":"+t.tpe)
        typed{gen.mkAsInstanceOf(Ident(t),v.tpe)} // refinement
      }
    })
  }

  /** returns  `List[ Tuple2[ scala.Int, <elemType> ] ]' */
  final def SeqTraceType(elemType: Type): Type =
    appliedType(definitions.ListClass.typeConstructor, 
                List(pairType(definitions.IntClass.info,
                              elemType)))

  final def pairType(left: Type, right: Type) =
    appliedType(definitions.TupleClass(2).typeConstructor,
                List(left,right))

  /**  returns `Iterator[ elemType ]' */
  final def _seqIterType(elemType: Type): Type =
    appliedType(definitions.IteratorClass.typeConstructor,
                List(elemType))

  /** returns A for T <: Sequence[ A ]
   */
  final def getElemType_Sequence(tpe: Type): Type = {
    //System.err.println("getElemType_Sequence("+tpe.widen()+")")
    val tpe1 = tpe.widen.baseType(definitions.SeqClass)

    if (tpe1 == NoType)
      Predef.error("arg " + tpe + " not subtype of Seq[A]")

    tpe1.typeArgs(0)
  }

  // --------- these are new

  /** a faked switch statement
   */
  final def Switch(condition: Array[Tree], body: Array[Tree], defaultBody: Tree): Tree = {
    //assert condition != null:"cond is null";
    //assert body != null:"body is null";
    //assert defaultBody != null:"defaultBody is null";
    var result = defaultBody
    var i = condition.length - 1
    while (i >= 0) {
      result = If(condition(i), body(i), result)
      i -= 1
    }
    result
  }

  final def renamingBind(defaultv: Set[Symbol], scrut: Symbol, ndefault: Tree) = {
    if (!defaultv.isEmpty) {
      var dv: List[Symbol] = Nil
      var to: List[Symbol] = Nil
      val it = defaultv.elements; while(it.hasNext) { 
        dv = it.next :: dv
        to = scrut   :: to 
      }
      val tss = new TreeSymSubstituter(dv, to)
      tss.traverse(ndefault)
    } 
  }

  final def emptynessCheck(vsym: Symbol) = {
    if (vsym.tpe.typeSymbol == definitions.SomeClass)  // is Some[_]
      Literal(Constant(true))
    else                                          // is Option[_]
      Not(Select(Ident(vsym), nme.isEmpty))
  }

  final def makeIf(cond: Tree, thenp: Tree, elsep: Tree) = cond match {
    case Literal(Constant(true)) => thenp 
    case Literal(Constant(false)) => elsep
    case _ => If(cond, thenp, elsep)
  }

  /** returns code `<seqObj>.elements' */
  final def newIterator(seqObj: Tree): Tree = 
    Apply(Select(seqObj, newTermName("elements")), List())

  /** `it.next()'     */
  final def _next(iter: Tree) =
    Apply(Select(iter, definitions.Iterator_next), List())

  /** `it.hasNext()'  */
  final def _hasNext(iter: Tree) =  
    Apply(Select(iter, definitions.Iterator_hasNext), List())

  /** `!it.hasCur()'  */
  final def _not_hasNext(iter: Tree) = 
    Apply(Select(_hasNext(iter), definitions.Boolean_not), List())

  /** `trace.isEmpty' */
  final def isEmpty( iter: Tree  ):  Tree = 
    Apply(Select(iter, definitions.List_isEmpty), List())
 
  /** `arg.head' */
  final def SeqList_head(arg: Tree) = 
    Apply(Select(arg, definitions.List_head), List())

  final def Negate(tree: Tree) = tree match {
    case Literal(Constant(value:Boolean))=>
      Literal(Constant(!value))
    case _ =>
      Apply(Select(tree, definitions.Boolean_not), List());
  }

  /** for tree of sequence type, returns tree that drops first i elements */
  final def seqDrop(sel:Tree, i: Int) = if (i == 0) sel else 
    Apply(Select(Select(sel, "toList"), "drop"),
          List(Literal(Constant(i))))
  
  /** for tree of sequence type, returns boolean tree that has length i */
  final def seqHasLength(sel: Tree, ntpe: Type, i: Int) = 
    typed(
      Equals(
        Apply(Select(sel, ntpe.member(nme.length)), List()),
        Literal(Constant(i))
      )
    )/*defs.Seq_length ?*/

  /** for tree of sequence type sel, returns boolean tree testing that length >= i
   */
  final def seqLongerThan(sel:Tree, tpe:Type, i:Int) = 
    GreaterThanOrEquals(
      typed(Apply(Select(sel, tpe.member(nme.length)), List())),
      typed(Literal(Constant(i))))
      //defs.Seq_length instead of tpe.member ?

  final def Not(arg:Tree) = arg match {
    case Literal(Constant(true))  => Literal(Constant(false))
    case Literal(Constant(false)) => Literal(Constant(true))
    case t                        => Select(arg, definitions.Boolean_not)
  }
  /*protected*/ def And(left: Tree, right: Tree): Tree = left match {
    case Literal(Constant(value: Boolean)) =>
      if (value) right else left
    case _ => 
      right match {
        case Literal(Constant(true)) =>
	  left
        case _ =>
          Apply(Select(left, definitions.Boolean_and), List(right))
      }
  }

  /*protected*/final def Or(left: Tree, right: Tree): Tree = {
    left match {
/*
      case If(cond: Tree, thenp: Tree, Literal(Constant(false))) =>  // little opt, frequent special case
        If(cond, thenp, right)
*/
      case Literal(Constant(value: Boolean))=>    
	if(value) left else right
      case _ =>
        right match {
          case Literal(Constant(false)) =>
	    left
          case _ =>
            Apply(Select(left, definitions.Boolean_or), List(right));
        }
    }
  }
  
  // used by Equals
  /*
  private def getCoerceToInt(left: Type): Symbol = {
    val sym = left.nonPrivateMember( nme.coerce );
    //assert sym != Symbol.NONE : Debug.show(left);

    sym.alternatives.find {
      x => x.info match {
        case MethodType(vparams, restpe) =>
          vparams.length == 0 && isSameType(restpe,definitions.IntClass.info)
      }
    }.get
  }
  */
  // used by Equals
/*
  private def getEqEq(left: Type, right: Type): Symbol = {
    //Console.println("getEqeq of left  ==  "+left);
    val sym = left.nonPrivateMember( nme.EQEQ );


    //if (sym == NoSymbol)
    //  error("no eqeq for "+left);
    //    : Debug.show(left) + "::" + Debug.show(left.members());

    var fun: Symbol  = null;
    var ftype:Type  = null; // faster than `definitions.AnyClass.tpe'
    sym.alternatives.foreach {
      x => 
        //Console.println("getEqEq: "+x);
        val vparams = x.info.paramTypes;
        //Console.println("vparams.length ==  "+vparams.length);
      
        if (vparams.length == 1) {
          val vptype = vparams(0); 
          //Console.println("vptype ==  "+vptype);
          //Console.println("   ftype ==  "+ftype);
          //Console.println("   cond1 ==  "+isSubType(right, vptype));
          //Console.println("   cond2("+vptype+","+ftype+") ==  "+(ftype == null || isSubType(vptype, ftype)));
          //Console.println("vptype.getClass "+vptype.getClass());
          if (isSubType(right, vptype) && (ftype == null || isSubType(vptype, ftype)) ) {
            fun = x;
            ftype = vptype;
            //Console.println("fun now: "+fun+"  ftype now "+ftype);
          }
        }
    }
    //if (fun == null) scala.Predef.error("couldn't find eqeq for left"+left);
    fun;
  }
*/
  final def Equals(left: Tree, right: Tree): Tree =
    Apply(Select(left, nme.EQEQ), List(right))

  final def Eq(left: Tree, right: Tree): Tree =
    Apply(Select(left, nme.eq), List(right))

  final def GreaterThanOrEquals(left: Tree, right: Tree): Tree =
    Apply(Select(left, nme.GE), List(right))

  final def ThrowMatchError(pos: Position, obj: Tree) =
    atPos(pos) {
      Throw(
        New(
          TypeTree(definitions.MatchErrorClass.tpe),
          List(List(
            obj
          ))))
    }

  final def NotNull(tree:Tree) = 
    typed {
      Apply(Select(tree, nme.ne), List(Literal(Constant(null))))
    }

  final def IsNull(tree:Tree) = 
    typed {
      Apply(Select(tree, nme.eq), List(Literal(Constant(null))))
    }

    // statistics
    var nremoved = 0
    var nsubstituted = 0
    var nstatic = 0

  final def squeezedBlock(vds: List[Tree], exp: Tree)(implicit theOwner: Symbol): Tree = 
    if (settings_squeeze)
      squeezedBlock1(vds, exp)
    else
      Block(vds,exp)
  
  final def squeezedBlock1(vds: List[Tree], exp: Tree)(implicit theOwner: Symbol): Tree = {
    val tpe = exp.tpe

    class RefTraverser(sym: Symbol) extends Traverser {
      var nref = 0
      var nsafeRef = 0
      override def traverse(tree: Tree) = tree match {
        case t:Ident if t.symbol == sym => 
          nref += 1
          if(sym.owner == currentOwner)  { // oldOwner should match currentOwner
            nsafeRef += 1
          } /*else if(nref == 1) {
            Console.println("sym owner: "+sym.owner+" but currentOwner = "+currentOwner)
          }*/
        case t if nref > 1 =>
          // abort, no story to tell
        case t =>
          super.traverse(t)
      }
    }

    class Subst(sym: Symbol, rhs: Tree) extends Transformer {
      var stop = false
      override def transform(tree: Tree) = tree match {
        case t:Ident if t.symbol == sym => 
          stop = true
          rhs
        case t if stop =>
          t
        case t => 
          super.transform(t)
      }
    }
    vds match {
      case Nil =>
        exp
      case (vd:ValDef) :: rest =>
        // recurse
        val exp1 = squeezedBlock(rest, exp)

        //Console.println("squeezedBlock for valdef "+vd)
        val sym = vd.symbol
        val rt = new RefTraverser(sym)
        rt.atOwner (theOwner) (rt.traverse(exp1))
        rt.nref match {
          case 0 =>
            nremoved = nremoved + 1
            exp1
          case 1 if rt.nsafeRef == 1 =>
            nsubstituted += 1
            new Subst(sym, vd.rhs).transform(exp1)
          case _ => 
            exp1 match {
              case Block(vds2, exp2) => Block(vd::vds2, exp2)  
              case exp2              => Block(vd::Nil,  exp2)  
            }
        }
      case x::xs => 
        squeezedBlock(xs, exp) match {
          case Block(vds2, exp2) => Block(x::vds2, exp2) 
          case exp2              => Block(x::Nil,  exp2) 
        }
    }
  }


}

