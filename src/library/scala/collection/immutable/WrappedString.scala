/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable

import generic._
import mutable.{Builder, StringBuilder}
import scala.util.matching.Regex

/**
 *  This class serves as a wrapper augmenting `String`s with all the operations
 *  found in indexed sequences.
 *  
 *  The difference between this class and `StringOps` is that calling transformer
 *  methods such as `filter` and `map` will yield an object of type `WrappedString` 
 *  rather than a `String`.
 *  
 *  @param self    a string contained within this wrapped string
 *  
 *  @since 2.8
 *  @define Coll WrappedString
 *  @define coll wrapped string
 */
class WrappedString(val self: String) extends IndexedSeq[Char] with StringLike[WrappedString] {

  override protected[this] def thisCollection: WrappedString = this
  override protected[this] def toCollection(repr: WrappedString): WrappedString = repr

  /** Creates a string builder buffer as builder for this class */
  override protected[this] def newBuilder = WrappedString.newBuilder
  
  override def slice(from: Int, until: Int): WrappedString = 
    new WrappedString(self.substring(from max 0, until min self.length))
  override def toString = self
}

/** A companion object for wrapped strings.
 *  
 *  @since 2.8
 */
object WrappedString {
  def newBuilder: Builder[Char, WrappedString] = StringBuilder.newBuilder mapResult (x => new WrappedString(x))
}
