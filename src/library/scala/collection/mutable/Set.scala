/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable

import generic._

/** A base trait for sets that can be mutated.
 *  $setNote
 *  $setTags
 *  @since 1.0
 *  @author Matthias Zenger
 */
trait Set[A] extends Iterable[A]
                with scala.collection.Set[A]
                with GenericSetTemplate[A, Set]
                with SetLike[A, Set[A]] {
  override def companion: GenericCompanion[Set] = Set
}
                
/** $factoryInfo
 *  The current default implementation of a $Coll is a `HashSet`.
 *  @define coll mutable set
 *  @define Coll mutable.Set
 */
object Set extends MutableSetFactory[Set] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Set[A]] = setCanBuildFrom[A]
  override def empty[A]: Set[A] = HashSet.empty[A]
}

