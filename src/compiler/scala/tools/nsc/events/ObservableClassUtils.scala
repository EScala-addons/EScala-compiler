package scala.tools.nsc
package events

import ast.parser.TreeBuilder

trait ObservableClassUtil {
  self: ObservableClass =>

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

  private def tupleize(params: List[Tree]) = 

    if(params.size > 1)
      genTupleType(params)
    else
      params

  /*
   * Will crate a Tree representing the type:
   * scala.events.AllObject[tparams]
   */
  protected[events] def genAllObjectTpt(tparams: Tree) = {
    AppliedTypeTree(
      Select(
        Select(
          Ident("scala"),
          newTermName("events")
        ),
        newTypeName("AllObject")
      ),
    List(tparams))
  }

  /*
   * Symbol allInstances[C]
   */
  lazy val ModEvents = definitions.getModule("scala.events")
  lazy val MethAllInstances = definitions.getMember(ModEvents,
                                                    "allInstances")

  
//  protected[events] def newAllObject(tparams: List[Tree], exec: Name) = {
//    val dependentType =
//      Select(
//        Ident(exec),
//        newTypeName("AllObject")) /* TODO is that name ok ? is it important ? */
//    makeNew(genAllObjectTpt(tparams) :: dependentType :: Nil, emptyValDef, Nil, List(Nil), NoPosition, NoPosition)
//  }
//
}
