/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
import generic._

/** A sorted set.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.4
 */
trait SortedSet[A] extends Set[A] with SortedSetLike[A, SortedSet[A]] {
  /** Needs to be overridden in subclasses. */
  override def empty: SortedSet[A] = throw new UnsupportedOperationException("SortedMap.empty")
}

/**
 * @since 2.8
 */
object SortedSet extends ImmutableSortedSetFactory[immutable.SortedSet] {
  implicit def builderFactory[A](implicit ord: Ordering[A]): BuilderFactory[A, SortedSet[A], Coll] = new SortedSetBuilderFactory[A]
  def empty[A](implicit ord: Ordering[A]): immutable.SortedSet[A] = immutable.SortedSet.empty[A](ord)
}



