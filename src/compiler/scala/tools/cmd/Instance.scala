/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package cmd

/** The trait mixed into each instance of a specification.
 *
 *  @see    Reference
 */
trait Instance extends Spec {
  def parsed: CommandLine

  protected def help(str: => String): Unit = ()

  def isSet(s: String)    = parsed isSet toOpt(s)
  def originalArgs        = parsed.originalArgs
  
  type OptionMagic = Opt.Instance
  protected implicit def optionMagicAdditions(name: String) = new Opt.Instance(programInfo, parsed, name)
}
