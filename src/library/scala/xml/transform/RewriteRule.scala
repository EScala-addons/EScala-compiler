/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.transform;


/** a RewriteRule, when applied to a term, yields either
 *  the resulting of rewriting or the term itself it the rule
 *  is not applied
 */
abstract class RewriteRule extends BasicTransformer {
  /** a name for this rewrite rule */
  val name = this.toString();
  override def transform(ns:Seq[Node]): Seq[Node] = super.transform(ns);
  override def transform(n:Node): Seq[Node] = n;
}

