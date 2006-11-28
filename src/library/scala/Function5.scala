
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// generated on Tue Nov 28 14:03:53 CET 2006  
// $Id$

package scala


/**
 * Function with 5 parameters. 
 */
trait Function5 [-T1, -T2, -T3, -T4, -T5, +R] extends AnyRef {
  def apply(v1:T1, v2:T2, v3:T3, v4:T4, v5:T5): R
  override def toString() = "<function>"
  
}
