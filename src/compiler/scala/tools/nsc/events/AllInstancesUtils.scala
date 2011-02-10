package scala.tools.nsc
package events

import ast.parser.TreeBuilder

trait AllInstancesUtil {
  self: AllInstances =>

  val global: Global
  import global._
  import symtab.Flags._

  /*
   * Symbol allInstances[C]
   */
  lazy val ModEvents = definitions.getModule("scala.events")
  lazy val MethAllInstances = definitions.getMember(ModEvents,
                                                    "allInstances")
  lazy val MethAnyInstance = definitions.getMember(ModEvents,
                                                    "anyInstance")
}
