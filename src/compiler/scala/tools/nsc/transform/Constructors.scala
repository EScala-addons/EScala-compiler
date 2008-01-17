/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author
 */
// $Id$

package scala.tools.nsc.transform

import scala.collection.mutable.ListBuffer
import symtab.Flags._
import util.TreeSet

abstract class Constructors extends Transform {
  import global._
  import definitions._
  import posAssigner.atPos

  /** the following two members override abstract members in Transform */
  val phaseName: String = "constructors"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ConstructorTransformer

  class ConstructorTransformer extends Transformer {

    def transformClassTemplate(impl: Template): Template = {
      val clazz = impl.symbol.owner
      val stats = impl.body
      val localTyper = typer.atOwner(impl, clazz)
      var constr: DefDef = null
      var constrParams: List[Symbol] = null
      var constrBody: Block = null
      // decompose primary constructor into the three entities above.
      for (stat <- stats) {
        stat match {
          case ddef @ DefDef(_, _, _, List(vparams), _, rhs @ Block(_, Literal(_))) =>
            if (ddef.symbol.isPrimaryConstructor) {
              constr = ddef
              constrParams = vparams map (_.symbol)
              constrBody = rhs
            }
          case _ =>
        }
      }
      assert((constr ne null) && (constrBody ne null), impl)

      val paramAccessors = clazz.constrParamAccessors

      def parameter(acc: Symbol): Symbol = 
        parameterNamed(nme.getterName(acc.originalName))

      def parameterNamed(name: Name): Symbol = {
        def matchesName(param: Symbol) = 
          param.name == name ||
          param.name.startsWith(name) && param.name(name.length) == '$'
        val ps = constrParams filter matchesName
        if (ps.isEmpty) assert(false, "" + name + " not in " + constrParams)
        ps.head
      }

      var thisRefSeen: Boolean = false

      val intoConstructorTransformer = new Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Apply(Select(This(_), _), List()) =>
            if ((tree.symbol hasFlag PARAMACCESSOR) && tree.symbol.owner == clazz)
              gen.mkAttributedIdent(parameter(tree.symbol.accessed)) setPos tree.pos
            else if (tree.symbol.outerSource == clazz && !clazz.isImplClass)
              gen.mkAttributedIdent(parameterNamed(nme.OUTER)) setPos tree.pos
            else 
              super.transform(tree)
          case Select(This(_), _)
          if ((tree.symbol hasFlag PARAMACCESSOR) && !tree.symbol.isSetter && tree.symbol.owner == clazz) =>
            gen.mkAttributedIdent(parameter(tree.symbol)) setPos tree.pos
          case Select(_, _) =>
            thisRefSeen = true
            super.transform(tree)
          case This(_) =>
            thisRefSeen = true
            super.transform(tree)
          case Super(_, _) =>
            thisRefSeen = true
            super.transform(tree)
          case _ =>
            super.transform(tree)
        }
      }

      def intoConstructor(oldowner: Symbol, tree: Tree) =
        intoConstructorTransformer.transform(
          new ChangeOwnerTraverser(oldowner, constr.symbol)(tree))

      def canBeMoved(tree: Tree) = tree match {
        //todo: eliminate thisRefSeen
        case ValDef(mods, _, _, _) => (mods hasFlag PRESUPER | PARAMACCESSOR) || !thisRefSeen
        case _ => false
      }

      def mkAssign(to: Symbol, from: Tree): Tree =
        atPos(to.pos) {
          localTyper.typed {
            Assign(Select(This(clazz), to), from)
          }
        }

      def copyParam(to: Symbol, from: Symbol): Tree = {
        var result = mkAssign(to, Ident(from))
        if (from.name == nme.OUTER)
          result =
            atPos(to.pos) {
              localTyper.typed {
                If(Apply(Select(Ident(from), nme.eq), List(Literal(Constant(null)))),
                   Throw(New(TypeTree(NullPointerExceptionClass.tpe), List(List()))),
                   result);
              }
            }
        result
      }

      val defBuf = new ListBuffer[Tree]
      val constrStatBuf = new ListBuffer[Tree]
      val constrPrefixBuf = new ListBuffer[Tree]
      val presupers = treeInfo.preSuperFields(stats)
      for (stat <- constrBody.stats) {
        constrStatBuf += stat
        stat match {
          case ValDef(mods, name, _, _) if (mods hasFlag PRESUPER) =>
            constrStatBuf += 
              localTyper.typed {
                atPos(stat.pos) {
                  val fields = presupers filter (
                    vdef => nme.localToGetter(vdef.name) == name)
                  assert(fields.length == 1)
                  Assign(
                    gen.mkAttributedRef(clazz.thisType, fields.head.symbol),
                    Ident(stat.symbol))
                }
              }
          case _ =>
        }
      }

      for (stat <- stats) stat match {
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          stat.symbol.tpe match {
            case MethodType(List(), tp @ ConstantType(c)) =>
              defBuf += copy.DefDef(
                stat, mods, name, tparams, vparamss, tpt,
                Literal(c) setPos rhs.pos setType tp)
            case _ =>
              if (!stat.symbol.isPrimaryConstructor) defBuf += stat
          }
        case ValDef(mods, name, tpt, rhs) =>
          if (stat.symbol.tpe.isInstanceOf[ConstantType])
            assert(stat.symbol.getter(stat.symbol.owner) != NoSymbol, stat)
          else {
           try {
            if (rhs != EmptyTree && !stat.symbol.hasFlag(LAZY)) {
              val rhs1 = intoConstructor(stat.symbol, rhs);
              (if (canBeMoved(stat)) constrPrefixBuf else constrStatBuf) += mkAssign(
                stat.symbol, rhs1)
            }
            defBuf += copy.ValDef(stat, mods, name, tpt, EmptyTree)
           } catch {
             case ex: Throwable =>
               println("error when transforming "+stat+" in "+stats)
               throw ex
           }
          }
        case ClassDef(_, _, _, _) =>
          defBuf += (new ConstructorTransformer).transform(stat)
        case _ =>
          constrStatBuf += intoConstructor(impl.symbol, stat)
      }

      val accessed = new TreeSet[Symbol]((x, y) => x isLess y)

      def isAccessed(sym: Symbol) =
        sym.owner != clazz ||
        !(sym hasFlag PARAMACCESSOR) ||
        !sym.isPrivateLocal ||
        (accessed contains sym)

      val accessTraverser = new Traverser {
        override def traverse(tree: Tree) = {
          tree match {
            case Select(_, _) =>
              if (!isAccessed(tree.symbol)) accessed addEntry tree.symbol
            case _ =>
          }
          super.traverse(tree)
        }
      }

      for (stat <- defBuf.elements) accessTraverser.traverse(stat)

      val paramInits = for (acc <- paramAccessors if isAccessed(acc))
                       yield copyParam(acc, parameter(acc))

      defBuf += copy.DefDef(
        constr, constr.mods, constr.name, constr.tparams, constr.vparamss, constr.tpt,
        copy.Block(
          constrBody,
          paramInits ::: constrPrefixBuf.toList ::: constrStatBuf.toList,
          constrBody.expr));

      for (sym <- clazz.info.decls.toList) 
        if (!isAccessed(sym)) clazz.info.decls unlink sym

      copy.Template(impl, impl.parents, impl.self, 
                    defBuf.toList filter (stat => isAccessed(stat.symbol)))
    }

    override def transform(tree: Tree): Tree = 
      tree match {
        case ClassDef(mods, name, tparams, impl) if !tree.symbol.hasFlag(INTERFACE) =>
          copy.ClassDef(tree, mods, name, tparams, transformClassTemplate(impl))
        case _ =>
          super.transform(tree)
      }

  } // ConstructorTransformer

}
