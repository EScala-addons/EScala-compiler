package scala.tools.nsc
package events

trait ObservableClassUtil {
  self: ObservableClass =>
  val global: Global
  import global._
  import symtab.Flags._

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

}
