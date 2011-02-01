package scala.tools.nsc
package events

import ast.parser.TreeBuilder

trait AllInstancesUtil {
  self: AllInstances =>

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
    def freshTypeName(prefix: String) = newTypeName(prefix)
    def freshTermName(prefix: String) = freshName(prefix)
    override def freshName() = freshName("events$")
    def o2p(offset: Int) = NoPosition
    def r2p(start: Int, point: Int, end: Int) = NoPosition
  }

  import treeBuilder._

  /*
   * Symbol allInstances[C]
   */
  lazy val ModEvents = definitions.getModule("scala.events")
  lazy val MethAllInstances = definitions.getMember(ModEvents,
                                                    "allInstances")

}
